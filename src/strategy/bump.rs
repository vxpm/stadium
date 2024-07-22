//! The bump strategy.
//!
//! It is the simplest possible strategy. Backed by a [`Vec`], the id of an inserted value is it's
//! index. Elements cannot be removed.

use super::{
    common::{map_vec, Index},
    Strategy, StrategyExtClear, StrategyExtMap, StrategyKind,
};
use std::marker::PhantomData;

pub struct Bump<I>(PhantomData<I>);
// SAFETY: invariant is upheld
unsafe impl<I> StrategyKind for Bump<I>
where
    I: Index,
{
    type Id = I;
    type Strategy<T> = BumpStrat<T, I>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct BumpStrat<T, I> {
    slots: Vec<T>,
    phantom: PhantomData<I>,
}

impl<T, I> Default for BumpStrat<T, I> {
    fn default() -> Self {
        Self {
            slots: Vec::new(),
            phantom: PhantomData,
        }
    }
}

// SAFETY: invariant is upheld
unsafe impl<T, I> Strategy<T> for BumpStrat<T, I>
where
    I: Index,
{
    type Kind = Bump<I>;

    #[inline(always)]
    fn len(&self) -> usize {
        self.slots.len()
    }

    #[inline(always)]
    fn get(&self, id: <Self::Kind as StrategyKind>::Id) -> Option<&T> {
        self.slots.get(id.to_usize())
    }

    #[inline(always)]
    fn get_mut(&mut self, id: <Self::Kind as StrategyKind>::Id) -> Option<&mut T> {
        self.slots.get_mut(id.to_usize())
    }

    #[inline(always)]
    fn insert(&mut self, value: T) -> <Self::Kind as StrategyKind>::Id {
        let index = self.slots.len();
        self.slots.push(value);

        I::from_usize(index)
    }

    #[inline(always)]
    fn iter<'this>(
        &'this self,
    ) -> impl Iterator<Item = (<Self::Kind as StrategyKind>::Id, &'this T)>
    where
        T: 'this,
    {
        self.slots
            .iter()
            .enumerate()
            .map(|(i, v)| (I::from_usize(i), v))
    }

    #[inline(always)]
    fn iter_mut<'this>(
        &'this mut self,
    ) -> impl Iterator<Item = (<Self::Kind as StrategyKind>::Id, &'this mut T)>
    where
        T: 'this,
    {
        self.slots
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (I::from_usize(i), v))
    }
}

impl<T, I> StrategyExtClear<T> for BumpStrat<T, I>
where
    I: Index,
{
    #[inline(always)]
    fn clear(&mut self) {
        self.slots.clear();
    }
}

impl<T, I> StrategyExtMap<T> for BumpStrat<T, I>
where
    I: Index,
{
    #[inline(always)]
    fn map<U, F>(self, f: F) -> <Self::Kind as StrategyKind>::Strategy<U>
    where
        F: FnMut(T) -> U,
    {
        BumpStrat::<U, I> {
            slots: map_vec(self.slots, f),
            phantom: PhantomData,
        }
    }
}

#[cfg(test)]
mod test {
    use super::Bump;
    use crate::Stadium as RawStadium;

    type Stadium<T> = RawStadium<T, Bump<u8>>;

    #[test]
    fn test_insert_get() {
        let mut stadium = Stadium::new();

        let hi = stadium.insert("hi");
        let hello = stadium.insert("hello");

        assert_eq!(stadium.get(hi), Some(&"hi"));
        assert_eq!(stadium.get(hello), Some(&"hello"));

        dbg!(stadium);
    }
}
