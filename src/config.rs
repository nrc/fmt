use indent::Offset;

pub struct Config {
    max_width: usize,
    tab: Offset,
    comments: CommentConfig,
}

impl Config {
    pub fn default() -> Config {
        Config {
            max_width: 100,
            tab: Offset::Spaces(4),
            comments: CommentConfig::rust_comments(),
        }
    }
}

struct CommentConfig {
    kinds: Vec<CommentKind>,
}

impl CommentConfig {
    fn rust_comments() -> CommentConfig {
        CommentConfig {
            kinds: vec![CommentKind::new("///", "///", ""),
                        CommentKind::new("//!", "//!", ""),
                        CommentKind::new("//", "//", ""),
                        CommentKind::new("/**", " **", " **/"),
                        CommentKind::new("/*!", " *", " */"),
                        CommentKind::new("/*", " *", " */")],
        }
    }
}

#[derive(new)]
struct CommentKind {
    opener: &'static str,
    line_start: &'static str,
    closer: &'static str,
}

// Things that will make finding comments hard
//     reordering (e.g., imports)
//     elided words or delimiters (e.g., removed block or pub for pub(self))
//     extra words (e.g., extern "C")
