use thiserror::Error;

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
    #[error("Invalid environment format: {0}")]
    InvalidEnvironmentFormat(String),
}
