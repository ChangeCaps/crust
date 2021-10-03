#[derive(Clone, thiserror::Error, Debug)]
pub enum RuntimeError {
    #[error("{{main}} has overflowed it's stack")]
    StackOverflow,

    #[error("attempted stack access out of bounds")]
    OutOfBounds,
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;
