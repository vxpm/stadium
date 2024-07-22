pub mod bump;
pub mod common;
pub mod gen;
pub mod risky;

#[expect(unused_imports, reason = "imported for rustdoc")]
use crate::{Id, Stadium};

/// A kind of strategy. See [`Strategy`].
///
/// # Safety
/// `Self::Strategy<T>::Kind` must be equal to Self.
pub unsafe trait StrategyKind {
    /// The identifier type used by this strategy kind. This will be wrapped in an opaque [`Id<T>`]
    /// by the [`Stadium`].
    type Id: Copy;

    /// The strategy instance type of this strategy kind. This is the type that actually implements
    /// the strategy for a [`Stadium<T>`].
    type Strategy<T>: Strategy<T>;
}

/// A strategy is an internal implementation for a [`Stadium`]. Each strategy has different
/// guarantees, pros and cons. See [`Stadium`] for more information.
///
/// # Safety
/// `Self::Kind::Strategy<T>` must be equal to Self.
pub unsafe trait Strategy<T>: Default {
    /// The kind of this strategy.
    type Kind: StrategyKind;

    /// The number of elements in the [`Stadium`].
    fn len(&self) -> usize;

    /// Whether the [`Stadium`] is empty or not.
    #[inline(always)]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a reference to the element associated with the given [`Id`].
    fn get(&self, id: <Self::Kind as StrategyKind>::Id) -> Option<&T>;
    /// Returns a mutable reference to the element associated with the given [`Id`].
    fn get_mut(&mut self, id: <Self::Kind as StrategyKind>::Id) -> Option<&mut T>;

    /// Inserts the given element into the [`Stadium`] and returns it's generated id.
    #[must_use]
    fn insert(&mut self, value: T) -> <Self::Kind as StrategyKind>::Id;

    fn iter<'this>(
        &'this self,
    ) -> impl Iterator<Item = (<Self::Kind as StrategyKind>::Id, &'this T)>
    where
        T: 'this;
    fn iter_mut<'this>(
        &'this mut self,
    ) -> impl Iterator<Item = (<Self::Kind as StrategyKind>::Id, &'this mut T)>
    where
        T: 'this;
}

pub trait StrategyExtClear<T>: Strategy<T> {
    /// Clears the [`Stadium`]. This is equivalent to removing every element in it.
    fn clear(&mut self);
}

pub trait StrategyExtRemove<T>: StrategyExtClear<T> {
    /// Removes the element with the given id from the [`Stadium`] and returns it.
    fn remove(&mut self, id: <Self::Kind as StrategyKind>::Id) -> Option<T>;
}

pub trait StrategyExtMap<T>: Strategy<T> {
    /// Maps a [`Stadium<T>`] to a [`Stadium<U>`] as long as `T` and `U` have the same size and
    /// alignment. This method will fail to compile if the requirements are not met.
    fn map<U, F>(self, f: F) -> <Self::Kind as StrategyKind>::Strategy<U>
    where
        F: FnMut(T) -> U;
}
