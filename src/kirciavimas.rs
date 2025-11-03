use std::{
    fmt::Display,
    io::pipe,
    os::fd::{AsRawFd, FromRawFd},
    process::{Command, Stdio},
};

use crate::html_ext::NodeExt;

use super::{CsrfToken, SESSION_NAME};

const SITE_URL: &str = "https://rasyba.lietuviuzodynas.lt/kirciavimas-internetu";
const ACCENTUATE_API_URL: &str = "https://rasyba.lietuviuzodynas.lt/api/accentuate/names";

fn new_session() -> CsrfToken {
    let (pipe_rx, pipe_tx) = pipe().expect("Unable to create pipe");

    // http --session=lietuviu-zodynas GET https://rasyba.lietuviuzodynas.lt/kirciavimas-internetu
    let http_response = Command::new("http")
        .arg("--session")
        .arg(SESSION_NAME)
        .arg("GET")
        .arg(SITE_URL)
        .stdout(unsafe { Stdio::from_raw_fd(pipe_tx.as_raw_fd()) })
        .stderr(Stdio::inherit())
        .output()
        .expect("Failed to wait for http with new session");

    assert!(
        http_response.status.success(),
        "HTTP request for establishing new session failed"
    );

    let csrf_token = Command::new("grep")
        .arg("-oP")
        .arg(r#"<meta name="csrf-token" content="\K[^"]+"#)
        .stdin(unsafe { Stdio::from_raw_fd(pipe_rx.as_raw_fd()) })
        .output()
        .expect("Failed to wait for grep");

    assert!(csrf_token.status.success(), "Failed to extract CSRF token");

    CsrfToken::new(String::from_utf8(csrf_token.stdout).expect("Failed to parse CSRF token"))
}

fn accentuate_api(
    word: &str,
    csrf_token: Option<CsrfToken>,
) -> Result<AccentuationOutput, InnerAccentuationError> {
    // http --session lietuviu-zodynas POST 'https://rasyba.lietuviuzodynas.lt/api/accentuate/names' text="$WORD"

    let mut cmd = Command::new("http");

    cmd.arg("--session")
        .arg(SESSION_NAME)
        .arg("--ignore-stdin")
        .arg("POST")
        .arg(ACCENTUATE_API_URL);

    if let Some(token) = csrf_token {
        cmd.arg(format!("x-csrf-token:{}", token.token));
    }

    cmd.arg(format!("text={word}"));

    let http_response = cmd
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("Failed to wait for http with accentuate API");

    assert!(
        http_response.status.success(),
        "HTTP request for accentuating word failed: {:#?}",
        http_response
    );

    let html =
        String::from_utf8(http_response.stdout).expect("Failed to parse API accentuation stdout");

    if html.contains("CSRF token mismatch.") {
        return Err(InnerAccentuationError::SessionExpired);
    }

    let dom = html_parser::Dom::parse(&html).expect("Failed to parse HTML");

    std::panic::catch_unwind(|| AccentuationOutput::try_from_html(&dom)).unwrap_or_else(|panic| {
        eprintln!("Failed to parse accentuation output. DOM: {:#?}", dom);
        std::panic::resume_unwind(panic);
    })
}

pub fn accentuate_word(
    word: &str,
    mut start_new_session: bool,
) -> Result<AccentuationOutput, AccentuationError> {
    loop {
        let maybe_csrf_token = start_new_session.then(|| {
            // Starting a new session.
            eprintln!("Pradedama nauja sesija...");
            new_session()
        });

        return match accentuate_api(word, maybe_csrf_token) {
            Ok(accentuated) => Ok(accentuated),
            Err(InnerAccentuationError::NoSuchWord) => Err(AccentuationError::NoSuchWord),
            Err(InnerAccentuationError::SessionExpired) => {
                if start_new_session {
                    // Server still rejects our request, even after starting a new session.
                    Err(AccentuationError::ServerError)
                } else {
                    eprintln!("Sesijos pasibaigė! Nauja sesija bus pradėta.");
                    start_new_session = true;
                    continue;
                }
            }
            Err(InnerAccentuationError::ServerError) => Err(AccentuationError::ServerError),
        };
    }
}

#[derive(Debug)]
pub struct AccentuationOutput {
    pub variants: Vec<AccentuationVariant>,
}

#[derive(Debug)]
pub struct AccentuationVariant {
    pub accentuated_word: String,
    pub valid_forms: Vec<GrammaticalForm>,
}

#[derive(Debug)]
enum InnerAccentuationError {
    NoSuchWord,
    SessionExpired,
    ServerError,
}

#[derive(Debug)]
pub enum AccentuationError {
    NoSuchWord,
    ServerError,
}

impl AccentuationOutput {
    fn try_from_html(dom: &html_parser::Dom) -> Result<Self, InnerAccentuationError> {
        let first_child = dom.children.first().unwrap();
        if let Some(first_child_element) = first_child.element() {
            return Ok(first_child_element.into());
        }

        if let Some(text) = first_child.text() {
            if text.contains("žodis nerastas") {
                return Err(InnerAccentuationError::NoSuchWord);
            }

            if text.contains("Server Error") {
                return Err(InnerAccentuationError::ServerError);
            }
        }

        panic!("Unexpected DOM structure: {:#?}", dom);
    }
}

impl From<&html_parser::Element> for AccentuationOutput {
    fn from(value: &html_parser::Element) -> Self {
        let (chunks, remainder) = value.children.as_chunks();

        let variants = chunks
            .iter()
            .map(|[word, morf]| {
                let accentuated_word = word
                    .extract_self_or_first_child_as_text()
                    .unwrap()
                    .to_owned();
                let forms = morf
                    .element()
                    .unwrap()
                    .children
                    .iter()
                    .map(|form| GrammaticalForm::from(form.element().unwrap()))
                    .collect();
                AccentuationVariant {
                    accentuated_word,
                    valid_forms: forms,
                }
            })
            .chain(remainder.first().map(|word| {
                let accentuated_word = word
                    .extract_self_or_first_child_as_text()
                    .unwrap()
                    .to_owned();
                AccentuationVariant {
                    accentuated_word,
                    valid_forms: Vec::new(),
                }
            }))
            .collect();

        Self { variants }
    }
}

#[derive(Debug)]
pub struct GrammaticalForm {
    pub form: String,
}

impl From<&html_parser::Element> for GrammaticalForm {
    fn from(value: &html_parser::Element) -> Self {
        Self {
            form: value
                .children
                .first()
                .and_then(|child| child.text())
                .unwrap_or("<Erroneous grammatical form>")
                .to_owned(),
        }
    }
}

impl AccentuationOutput {
    pub fn as_tabled(&self) -> AccentuationTabled<'_> {
        AccentuationTabled {
            accentuation_output: self,
        }
    }
}

pub struct AccentuationTabled<'a> {
    accentuation_output: &'a AccentuationOutput,
}

impl Display for AccentuationTabled<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = tabled::builder::Builder::new();

        for variant in &self.accentuation_output.variants {
            builder.push_record([
                variant.accentuated_word.clone(),
                variant
                    .valid_forms
                    .iter()
                    .map(|form| form.form.as_str())
                    .collect::<Vec<_>>()
                    .join("\n"),
            ]);
            // builder.push_column();
        }

        let mut table = builder.build();
        table
            .with(tabled::settings::Style::rounded())
            .with(tabled::settings::Alignment::center())
            .with(tabled::settings::Alignment::center_vertical())
            .with(tabled::settings::Padding::new(3, 3, 1, 1));

        std::fmt::Display::fmt(&table, f)
    }
}
