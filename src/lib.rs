#[macro_use]
extern crate derive_new;

mod engine;
mod config;
mod layout;
mod indent;

pub use engine::FmtEngine;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
