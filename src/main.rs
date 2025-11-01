use lietuviu_zodynas::{accentuate_api, new_session};

const WORD_ARG: &str = "word";
const NEW_SESSION_ARG: &str = "new_session";

fn main() {
    let make_cmd = || {
        clap::Command::new("Lietuvių Žodynas")
            .version("0.0.1")
            .author("Wojciech Przytuła")
            .about("A command-line tool for Lithuanian word accentuation")
            .arg_required_else_help(true)
            .arg(
                clap::Arg::new(WORD_ARG)
                    .help("The word to be accented")
                    .required(true), // .index(1),
            )
            .arg(
                clap::Arg::new(NEW_SESSION_ARG)
                    .help("Start a new session")
                    .short('n')
                    .long("new-session")
                    .num_args(0)
                    .action(clap::ArgAction::SetTrue),
            )
    };

    let matches = make_cmd().get_matches();

    // Get the only positional argument passed to the program
    let Some(word): Option<&String> = matches.get_one(WORD_ARG) else {
        let _ = make_cmd().print_long_help();
        return;
    };

    let maybe_csrf_token = matches.get_flag(NEW_SESSION_ARG).then(|| {
        eprintln!("Starting a new session...");
        new_session()
    });

    let Some(accentuated) = accentuate_api(word, maybe_csrf_token) else {
        eprintln!("Tokio žodžio nėra!");
        return;
    };
    println!("{}", accentuated.as_tabled());
}
