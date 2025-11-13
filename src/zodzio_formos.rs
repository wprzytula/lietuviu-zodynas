use std::{
    fmt::Display,
    process::{Command, Stdio},
};

use tabled::{Table, Tabled};

use crate::{
    html_ext::{ElementExt as _, NodeExt as _},
    kirciavimas::{
        AccentuationError, AccentuationOutput, Case, FitCriteria, Gender, NameForms, Number,
        PartOfSpeech, accentuate_words,
    },
};

use super::SESSION_NAME;

const MORFOLOGY_API_URL: &str = "https://morfologija.lietuviuzodynas.lt/zodzio-formos";

pub fn zodzio_formos(zodis: &str) -> Result<MorfologyOutput, MorfologyError> {
    // http -p hHb --pretty all GET "https://morfologija.lietuviuzodynas.lt/zodzio-formos/${WORD}"

    let mut cmd = Command::new("http");

    cmd.arg("--session")
        .arg(SESSION_NAME)
        .arg("GET")
        .arg(format!("{MORFOLOGY_API_URL}/{zodis}"));

    let http_response = cmd
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("Failed to wait for http with morfology API");

    assert!(
        http_response.status.success(),
        "HTTP request for morfology failed: {:#?}",
        http_response
    );

    let html =
        String::from_utf8(http_response.stdout).expect("Failed to parse API morfology stdout");

    let dom = html_parser::Dom::parse(&html).expect("Failed to parse HTML");
    if dom.tree_type != html_parser::DomVariant::Document {
        return Err(MorfologyError::NoSuchWord);
    }

    fn child_by_predicate<'a>(
        nodes: &'a [html_parser::Node],
        predicate: impl Fn(&html_parser::Element) -> bool,
    ) -> Option<&'a html_parser::Element> {
        nodes
            .iter()
            .find(|child| child.element().map_or(false, |elem| predicate(elem)))
            .and_then(|node| node.element())
    }

    fn child_by_name<'a>(
        nodes: &'a [html_parser::Node],
        name: &str,
    ) -> Option<&'a html_parser::Element> {
        child_by_predicate(nodes, |elem| elem.name == name)
    }

    fn child_by_class<'a>(
        nodes: &'a [html_parser::Node],
        class: &str,
    ) -> Option<&'a html_parser::Element> {
        child_by_predicate(nodes, |elem| elem.class_contains(class))
    }

    let html = child_by_name(&dom.children, "html").unwrap();
    let body = child_by_name(&html.children, "body").unwrap();
    let div = child_by_class(&body.children, "container").unwrap();
    let mut nodes = div.children.as_slice();
    loop {
        let (head, tail) = nodes.split_first().unwrap();
        nodes = tail;
        if head.element().is_some_and(|elem| elem.name == "nav") {
            break;
        }
    }
    if nodes
        .last()
        .and_then(|last_node| last_node.element())
        .is_some_and(|last_elem| last_elem.class_contains("top-a-block"))
    {
        nodes = nodes.split_last().unwrap().1;
    }

    std::panic::catch_unwind(|| MorfologyOutput::try_from_html(&nodes)).unwrap_or_else(|panic| {
        // eprintln!("Failed to parse morfology output. DOM: {:#?}", dom);
        std::panic::resume_unwind(panic);
    })
}

fn print_node(node: &html_parser::Node) {
    let text = match node {
        html_parser::Node::Element(elem) => &elem.source_span.text,
        html_parser::Node::Text(text) => text,
        html_parser::Node::Comment(_) => "",
    };

    println!("Node: {}", text);
}

#[derive(Debug)]
pub struct Gramemas {
    words: Vec<GramemoZodis>,
}

impl From<&html_parser::Element> for Gramemas {
    fn from(elem: &html_parser::Element) -> Self {
        let chunks = elem
            .children
            .chunk_by(|_fst, snd| !snd.element().unwrap().source_span.text.contains('→'));

        let words = chunks
            .into_iter()
            .map(|chunk| {
                let (word, forms) = chunk.split_first().unwrap();
                let word = word
                    .element()
                    .unwrap()
                    .extract_first_text_child()
                    .unwrap()
                    .trim_end_matches('→')
                    .trim_end()
                    .to_owned();
                let forms = forms
                    .iter()
                    .map(|node| {
                        let form_span = node.element().unwrap();
                        let form = form_span
                            .extract_first_text_child()
                            .unwrap()
                            .trim_start_matches('└')
                            .trim_start()
                            .to_owned();

                        GramemoForma { form }
                    })
                    .collect();
                GramemoZodis { word, forms }
            })
            .collect();

        Gramemas { words }
    }
}

#[derive(Debug)]
struct GramemoZodis {
    word: String,
    forms: Vec<GramemoForma>,
}

#[derive(Debug)]
struct GramemoForma {
    form: String,
}

#[derive(Debug)]
struct DeclensionTable {
    vardininkas: CaseDeclension,
    kilmininkas: CaseDeclension,
    naudininkas: CaseDeclension,
    galininkas: CaseDeclension,
    inagininkas: CaseDeclension,
    vietininkas: CaseDeclension,
    sauksmininkas: CaseDeclension,
}

impl DeclensionTable {
    pub fn as_tabled(&self) -> DeclensionTabled<'_> {
        DeclensionTabled {
            declension_table: self,
        }
    }

    fn accentuate(&mut self) {
        let declensions = [
            &mut self.vardininkas,
            &mut self.kilmininkas,
            &mut self.naudininkas,
            &mut self.galininkas,
            &mut self.inagininkas,
            &mut self.vietininkas,
            &mut self.sauksmininkas,
        ];
        let batch = CaseDeclension::batch_fetch_accentuations(&declensions).collect::<Vec<_>>();
        for (declension, accentuation) in declensions.into_iter().zip(batch) {
            if let Ok((vienaskaita_accentuation, daugiskaita_accentiuation)) = accentuation {
                declension.accentuate(&vienaskaita_accentuation, &daugiskaita_accentiuation);
            }
        }
    }
}

pub struct DeclensionTabled<'a> {
    declension_table: &'a DeclensionTable,
}

impl Display for DeclensionTabled<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut table = Table::new([
            &self.declension_table.vardininkas,
            &self.declension_table.kilmininkas,
            &self.declension_table.naudininkas,
            &self.declension_table.galininkas,
            &self.declension_table.inagininkas,
            &self.declension_table.vietininkas,
            &self.declension_table.sauksmininkas,
        ]);

        table
            .with(tabled::settings::Style::rounded())
            // .with(tabled::settings::Alignment::center())
            .with(tabled::settings::Alignment::center_vertical());
        // .with(tabled::settings::Padding::new(3, 3, 1, 1));

        table.fmt(f)
    }
}

impl Tabled for CaseDeclension {
    const LENGTH: usize = 2;

    fn fields(&self) -> Vec<std::borrow::Cow<'_, str>> {
        Vec::from(
            [
                self.case.as_str(),
                self.vienaskaita.as_str(),
                self.daugiskaita.as_str(),
            ]
            .map(Into::into),
        )
    }

    fn headers() -> Vec<std::borrow::Cow<'static, str>> {
        Vec::from(["Linksnis", "Vienaskaita", "Daugiskaita"].map(Into::into))
    }
}

impl From<&html_parser::Element> for DeclensionTable {
    fn from(table: &html_parser::Element) -> Self {
        assert!(table.class_contains("gramemas-table"));

        let mut rows = table
            .children
            .iter()
            .map(|node| node.element().unwrap())
            .inspect(|element| assert_eq!(element.name, "tr"));

        let _header_row = rows.next().unwrap();

        fn unpack_case(
            row: &html_parser::Element,
            case: Case,
            expected_case: &str,
        ) -> CaseDeclension {
            let [header, vienaskaita, daugiskaita] =
                <&[html_parser::Node; 3]>::try_from(row.children.as_slice()).unwrap();

            assert_eq!(
                header.extract_self_or_first_child_as_text(),
                Some(expected_case)
            );

            CaseDeclension {
                case,
                vienaskaita: vienaskaita
                    .extract_self_or_first_child_as_text_recursive()
                    .unwrap()
                    .to_owned(),
                daugiskaita: daugiskaita
                    .extract_self_or_first_child_as_text_recursive()
                    .unwrap()
                    .to_owned(),
            }
        }

        let vardininkas = unpack_case(rows.next().unwrap(), Case::Vardininkas, "V.");
        let kilmininkas = unpack_case(rows.next().unwrap(), Case::Kilmininkas, "K.");
        let naudininkas = unpack_case(rows.next().unwrap(), Case::Naudininkas, "K."); // Yes, this is a bug in the server which we must work around.
        let galininkas = unpack_case(rows.next().unwrap(), Case::Galininkas, "G.");
        let inagininkas = unpack_case(rows.next().unwrap(), Case::Inagininkas, "Įn.");
        let vietininkas = unpack_case(rows.next().unwrap(), Case::Vietininkas, "Vt.");
        let sauksmininkas = unpack_case(rows.next().unwrap(), Case::Sauksmininkas, "Š.");

        DeclensionTable {
            vardininkas,
            kilmininkas,
            naudininkas,
            galininkas,
            inagininkas,
            vietininkas,
            sauksmininkas,
        }
    }
}

#[derive(Debug)]
struct CaseDeclension {
    case: Case,
    vienaskaita: String,
    daugiskaita: String,
}

impl CaseDeclension {
    fn batch_fetch_accentuations(
        declensions: &[&mut Self],
    ) -> impl Iterator<Item = Result<(AccentuationOutput, AccentuationOutput), AccentuationError>>
    {
        let mut accentuated_batch = accentuate_words(
            declensions.iter().flat_map(|declension| {
                [
                    declension.vienaskaita.as_str(),
                    declension.daugiskaita.as_str(),
                ]
            }),
            false,
        );
        std::iter::from_fn(move || {
            let vienaskaita_accentuated = accentuated_batch.next()?;
            let daugiskaita_accentuated = accentuated_batch.next()?;
            Some(match (vienaskaita_accentuated, daugiskaita_accentuated) {
                (Ok(vienaskaita_accentuated), Ok(daugiskaita_accentuated)) => {
                    Ok((vienaskaita_accentuated, daugiskaita_accentuated))
                }
                (Err(err), _) => Err(err),
                (_, Err(err)) => Err(err),
            })
        })
    }

    fn accentuate(
        &mut self,
        vienaskaita_accentuation: &AccentuationOutput,
        daugiskaita_accentuation: &AccentuationOutput,
    ) {
        if let Some((vienaskaita_accentuated, _suitable_form)) = vienaskaita_accentuation
            .find_suitable_form(&FitCriteria {
                part_of_speech: PartOfSpeech::Noun(NameForms {
                    gender: Gender::Neuter, // This shall be ignored.
                    number: Number::Singular,
                    case: self.case,
                }),
            })
        {
            self.vienaskaita = vienaskaita_accentuated.to_owned();
        }

        if let Some((daugiskaita_accentuated, _suitable_form)) = daugiskaita_accentuation
            .find_suitable_form(&FitCriteria {
                part_of_speech: PartOfSpeech::Noun(NameForms {
                    gender: Gender::Neuter, // This shall be ignored.
                    number: Number::Plural,
                    case: self.case,
                }),
            })
        {
            self.daugiskaita = daugiskaita_accentuated.to_owned();
        }
    }
}

#[derive(Debug)]
pub struct MorfologyOutput {
    pub gramemas: Gramemas,
}

#[derive(Debug)]
pub enum MorfologyError {
    NoSuchWord,
    SessionExpired,
    ServerError,
}

impl MorfologyOutput {
    fn try_from_html(nodes: &[html_parser::Node]) -> Result<Self, MorfologyError> {
        let mut without_ads = nodes.iter().filter(|node| {
            node.element()
                .is_none_or(|elem| !elem.class_contains("top-a-block"))
        });

        let word_heading = without_ads.next().unwrap().element().unwrap();
        assert_eq!(word_heading.id.as_deref(), Some("word-heading"));

        let gramemas = without_ads.next().unwrap().element().unwrap();
        assert!(
            gramemas
                .classes
                .iter()
                .find(|class| *class == "gramemas")
                .is_some(),
        );

        let gramemas = Gramemas::from(gramemas);

        // without_ads.clone().for_each(|node| print_node(node));

        let declension_table_html = without_ads
            .next()
            .unwrap()
            .element()
            .unwrap()
            .children
            .first()
            .unwrap()
            .element()
            .unwrap();
        let mut declension_table = DeclensionTable::from(declension_table_html);
        // dbg!(&declension_table);
        println!("{}", declension_table.as_tabled());

        declension_table.accentuate();
        println!("{}", declension_table.as_tabled());

        Ok(Self { gramemas })
    }
}
