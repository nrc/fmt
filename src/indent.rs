use std::fmt::{Formatter, Display, Error};

#[derive(Copy, Clone, Debug)]
pub struct Indent {
    block: Offset,
    alignment: usize,
}

impl Indent {
    pub fn new() -> Indent {
        Indent {
            block: Offset::Spaces(0),
            alignment: 0,
        }
    }
}

impl Display for Indent {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.block)?;
        for _ in 0..self.alignment {
            write!(f, " ")?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Offset {
    Spaces(usize),
    Tabs(usize),
}

impl Display for Offset {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Offset::Spaces(n) => {
                for _ in 0..n {
                    write!(f, " ")?;
                }
                Ok(())
            }
            Offset::Tabs(n) => {
                for _ in 0..n {
                    write!(f, "\t")?;
                }
                Ok(())
            }
        }
    }
}
