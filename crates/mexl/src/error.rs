use thiserror::Error;

/// Represents errors that can occur in the Mexl library.
#[derive(Error, Debug, PartialEq)]
pub enum MexlError {
    #[error("Parser error: {0}")]
    ParseError(String),
    #[error("Compiler error: {0}")]
    CompileError(String),
    #[error("Runtime error: {0}")]
    RuntimeError(String),
    #[error("Cast error: {0}")]
    CastError(String),
}
