pub mod kirciavimas;

const SESSION_NAME: &str = "lietuviu-zodynas";

struct CsrfToken {
    token: String,
}

impl CsrfToken {
    fn new(mut token: String) -> Self {
        token.truncate(token.trim_end().len());
        CsrfToken { token }
    }
}
