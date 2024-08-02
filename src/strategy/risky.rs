//! The risky strategy.
//!
//! Backed by a [`Vec`], the id of an inserted value is it's index - just like the [`bump`]
//! strategy. However, unlike `bump`, this strategy allows removal. Since there's no way to
//! distinguish "dead" ids from valid ones, this strategy will suffer from the ABA problem if not
//! used correctly.
//!
//! [`bump`]: super::bump

use super::{
    common::{map_vec, Index},
    Strategy, StrategyExtClear, StrategyExtMap, StrategyExtRemove, StrategyKind,
};
use std::marker::PhantomData;

#[derive(Debug, Clone)]
enum Slot<T, I> {
    Empty(Option<I>),
    Occupied(T),
}

impl<T, I> Slot<T, I>
where
    I: Index,
{
    #[inline]
    fn new() -> Self {
        Self::Empty(None)
    }

    #[inline]
    fn as_value(&self) -> Option<&T> {
        if let Slot::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn as_value_mut(&mut self) -> Option<&mut T> {
        if let Slot::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn into_value(self) -> Option<T> {
        if let Slot::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn next_empty(&self) -> Option<I> {
        if let Slot::Empty(next) = self {
            *next
        } else {
            None
        }
    }
}

pub struct Risky<I>(PhantomData<I>);
// SAFETY: invariant is upheld
unsafe impl<I> StrategyKind for Risky<I>
where
    I: Index,
{
    type Id = I;
    type Strategy<T> = RiskyStrat<T, I>;
}

#[derive(Debug, Clone)]
pub struct RiskyStrat<T, I> {
    slots: Vec<Slot<T, I>>,
    occupied: I,
    empty_slots_head: Option<I>,
}

impl<T, I> RiskyStrat<T, I>
where
    I: Index,
{
    #[inline]
    fn get_slot_mut(&mut self, index: I) -> Option<&mut Slot<T, I>> {
        self.slots.get_mut(index.to_usize())
    }

    /// Acquires a slot from the top of the empty slot list and returns it's index. More
    /// specifically, this will:
    /// - Pop the slot index from the empty slots list
    /// - Update the empty stack to point to the next empty slot
    #[must_use = "the slot will be left unusable if not managed correctly"]
    fn acquire_slot_from_empty_list(&mut self) -> Option<I> {
        let empty_slots_head_index = self.empty_slots_head.take()?;

        // SAFETY: slots in the empty slots list must exist
        let empty_slots_head =
            unsafe { self.get_slot_mut(empty_slots_head_index).unwrap_unchecked() };
        self.empty_slots_head = empty_slots_head.next_empty();

        Some(empty_slots_head_index)
    }

    /// Acquires an available slot and returns its index. The slot at the index is guaranteed to
    /// not be a tombstone.
    #[inline]
    #[must_use = "the slot will be left unusable if not managed correctly"]
    fn acquire_slot(&mut self) -> I {
        self.acquire_slot_from_empty_list().unwrap_or_else(|| {
            self.slots.push(Slot::new());
            I::from_usize(self.slots.len() - 1)
        })
    }
}

impl<T, I> Default for RiskyStrat<T, I>
where
    I: Index,
{
    fn default() -> Self {
        Self {
            slots: Vec::new(),
            occupied: I::from_usize(0),
            empty_slots_head: None,
        }
    }
}

// SAFETY: invariant is upheld
unsafe impl<T, I> Strategy<T> for RiskyStrat<T, I>
where
    I: Index,
{
    type Kind = Risky<I>;

    #[inline(always)]
    fn len(&self) -> usize {
        self.slots.len()
    }

    #[inline(always)]
    fn get(&self, id: <Self::Kind as StrategyKind>::Id) -> Option<&T> {
        self.slots
            .get(id.to_usize())
            .and_then(|slot| slot.as_value())
    }

    #[inline(always)]
    fn get_mut(&mut self, id: <Self::Kind as StrategyKind>::Id) -> Option<&mut T> {
        self.slots
            .get_mut(id.to_usize())
            .and_then(|slot| slot.as_value_mut())
    }

    #[inline(always)]
    fn insert(&mut self, value: T) -> <Self::Kind as StrategyKind>::Id {
        self.occupied = self
            .occupied
            .next()
            .unwrap_or_else(|| panic!("exceeded maximum number of elements in the stadium"));
        let index = self.acquire_slot();

        // SAFETY: `acquire_slot` guarantees a slot exists at the given index and that it is not a
        // tombstone
        let slot = unsafe { self.get_slot_mut(index).unwrap_unchecked() };
        *slot = Slot::Occupied(value);

        index
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
            .filter_map(|slot| slot.as_value())
            .enumerate()
            .map(|(index, value)| (I::from_usize(index), value))
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
            .filter_map(|slot| slot.as_value_mut())
            .enumerate()
            .map(|(index, value)| (I::from_usize(index), value))
    }
}

impl<T, I> StrategyExtRemove<T> for RiskyStrat<T, I>
where
    I: Index,
{
    fn remove(&mut self, id: <Self::Kind as StrategyKind>::Id) -> Option<T> {
        self.occupied = self.occupied.previous()?;
        let empty_slots_head = std::mem::replace(&mut self.empty_slots_head, Some(id));

        let slot = self.get_slot_mut(id)?;
        if slot.as_value().is_none() {
            self.occupied = self.occupied.next().unwrap();
            self.empty_slots_head = empty_slots_head;
            return None;
        }

        // ok - slot contains a value
        let old_slot = std::mem::replace(slot, Slot::Empty(empty_slots_head));

        // SAFETY: the slot was already checked to be non-empty
        unsafe { Some(old_slot.into_value().unwrap_unchecked()) }
    }
}

impl<T, I> StrategyExtClear<T> for RiskyStrat<T, I>
where
    I: Index,
{
    #[inline(always)]
    fn clear(&mut self) {
        self.slots.clear();
    }
}

impl<T, I> StrategyExtMap<T> for RiskyStrat<T, I>
where
    I: Index,
{
    #[inline(always)]
    fn map<U, F>(self, mut f: F) -> <Self::Kind as StrategyKind>::Strategy<U>
    where
        F: FnMut(T) -> U,
    {
        let slots = map_vec(self.slots, |slot| match slot {
            Slot::Empty(next) => Slot::Empty(next),
            Slot::Occupied(value) => Slot::Occupied(f(value)),
        });

        RiskyStrat {
            slots,
            occupied: self.occupied,
            empty_slots_head: self.empty_slots_head,
        }
    }
}

#[cfg(test)]
mod test {
    use super::Risky;
    use crate::Stadium as RawStadium;

    type Stadium<T> = RawStadium<T, Risky<u8>>;

    #[test]
    fn test_insert_get() {
        let mut stadium = Stadium::new();

        let hi = stadium.insert("hi");
        let hello = stadium.insert("hello");

        assert_eq!(stadium.get(hi), Some(&"hi"));
        assert_eq!(stadium.get(hello), Some(&"hello"));

        dbg!(stadium);
    }

    #[test]
    #[should_panic]
    fn test_max_elements() {
        let mut stadium = Stadium::new();
        for _ in 0..u8::MAX {
            _ = stadium.insert("hi");
        }

        // one more!
        _ = stadium.insert("hi");
    }

    #[test]
    fn test_get() {
        let mut stadium = Stadium::new();
        let a = stadium.insert("hi");
        let b = stadium.insert("hello");

        assert_eq!(stadium.get(a), Some(&"hi"));
        assert_eq!(stadium.get(b), Some(&"hello"));

        *stadium.get_mut(a).unwrap() = "hello";
        *stadium.get_mut(b).unwrap() = "world";

        assert_eq!(stadium.get(a), Some(&"hello"));
        assert_eq!(stadium.get(b), Some(&"world"));
    }

    #[test]
    fn test_iter() {
        let mut stadium = Stadium::new();
        _ = stadium.insert("hi");
        _ = stadium.insert("hello");
        let mut iter = stadium.values();

        assert_eq!(iter.next(), Some(&"hi"));
        assert_eq!(iter.next(), Some(&"hello"));
    }

    #[test]
    fn test_remove_inconsistent() {
        let mut stadium_a = Stadium::new();
        let mut stadium_b = Stadium::new();

        let foo = stadium_a.insert("foo");
        let _bar = stadium_a.insert("bar");
        assert_eq!(stadium_a.remove(foo), Some("foo"));

        let baz = stadium_b.insert("baz");
        assert_eq!(stadium_b.remove(baz), Some("baz"));
        let baz = stadium_b.insert("baz");
        assert_eq!(stadium_a.remove(baz), None);
    }
}
