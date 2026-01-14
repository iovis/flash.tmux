use crossterm::style::{self, Attribute, Color, SetAttribute, SetForegroundColor, Stylize};

const BASE_TEXT_COLOR: Color = Color::Rgb {
    r: 148,
    g: 156,
    b: 187,
};

#[derive(Clone)]
pub struct Config {
    pub prompt_placeholder_text: String,
    pub prompt_indicator: String,
    pub label_characters: String,
    pub trimmable_chars: String,
    pub highlight_style: StyleSpec,
    pub current_style: StyleSpec,
    pub label_style: StyleSpec,
    pub prompt_style: StyleSpec,
    pub base_style: StyleSpec,
    pub style_sequences: StyleSequences,
}

impl Config {
    pub fn defaults() -> Self {
        Self {
            prompt_placeholder_text: "search...".to_string(),
            prompt_indicator: "❯".to_string(),
            label_characters: "asdfghjklqwertyuiopzxcvbnm".to_string(),
            trimmable_chars: "()[]{}\"'`".to_string(),
            highlight_style: StyleSpec::new(Some(Color::Rgb {
                r: 186,
                g: 187,
                b: 242,
            })),
            current_style: StyleSpec::new(Some(Color::Rgb {
                r: 239,
                g: 159,
                b: 119,
            })),
            label_style: StyleSpec::new(Some(Color::Rgb {
                r: 166,
                g: 209,
                b: 138,
            }))
            .bold(),
            prompt_style: StyleSpec::new(Some(Color::Magenta)).bold(),
            base_style: StyleSpec::new(Some(BASE_TEXT_COLOR)),
            style_sequences: StyleSequences::new(),
        }
    }
}

#[derive(Clone, Copy)]
#[must_use]
pub struct StyleSpec {
    fg: Option<Color>,
    bold: bool,
}

impl StyleSpec {
    pub fn new(fg: Option<Color>) -> Self {
        Self { fg, bold: false }
    }

    pub fn bold(mut self) -> Self {
        self.bold = true;
        self
    }

    pub fn apply(self, text: &str) -> String {
        let mut styled = style::style(text);
        if let Some(fg) = self.fg {
            styled = styled.with(fg);
        }
        if self.bold {
            styled = styled.attribute(Attribute::Bold);
        }
        format!("{styled}")
    }
}

#[derive(Clone)]
pub struct StyleSequences {
    pub reset: String,
    pub base: String,
}

impl StyleSequences {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            reset: format!("{}", SetAttribute(Attribute::Reset)),
            base: format!("{}", SetForegroundColor(BASE_TEXT_COLOR)),
        }
    }
}
