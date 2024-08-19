//! The generational strategy.
//!
//! Backed by a [`Vec`], the id of an inserted value is it's index _and_ it's generation. The
//! generation of an id is used to distinguish id's that map to the same index: each slot in the
//! stadium has it's own generation, and it is increased every time an element is removed from it.
//!
//! In other words, this strategy is like the [`risky`] strategy, but without being risky, as it
//! solves the ABA problem at the cost of memory usage.
//!
//! [`risky`]: super::risky

use super::common::{self, map_vec};
use super::StrategyExtMap;
use super::{common::Index, Strategy, StrategyExtClear, StrategyExtRemove, StrategyKind};
use std::marker::PhantomData;
use std::num::NonZero;

/// Trait for types that can be used as the generation of the slots.
///
/// # Safety
/// The sequence generated by `elem[n + 1] = elem[n].next()` must contain no repeated values and
/// have `elem[n + 1] > elem[n]` always hold.
pub unsafe trait Gen: super::common::sealed::Numeric {
    const FIRST: Self;

    /// Given the current generation, returns the next generation.
    fn next(self) -> Option<Self>;
}

macro_rules! impl_gen {
    ($($ty:ty),*) => {
        $(
            // SAFETY: invariant upheld
            unsafe impl Gen for $ty {
                const FIRST: Self = 0;

                #[inline(always)]
                fn next(self) -> Option<Self> {
                    self.checked_add(1)
                }
            }

            // SAFETY: invariant upheld
            unsafe impl Gen for NonZero<$ty> {
                const FIRST: Self = {
                    match NonZero::<$ty>::new(1) {
                        Some(x) => x,
                        None => unreachable!(),
                    }
                };

                #[inline(always)]
                fn next(self) -> Option<Self> {
                    self.checked_add(1)
                }
            }
        )*
    };
}

impl_gen! {
    u8,
    u16,
    u32,
    u64,
    u128
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Id<I, G> {
    index: I,
    gen: G,
}

impl<I, G> std::fmt::Debug for Id<I, G>
where
    I: common::Index,
    G: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}g{:?}", self.index.to_usize(), self.gen)
    }
}

#[derive(Debug, Clone)]
enum SlotState<T, I> {
    /// This slot is empty and might containt a pointer to the next empty slot on the stack.
    Empty(Option<I>),
    /// This slot contains a value.
    Occupied(T),
}

impl<T, I> SlotState<T, I>
where
    I: Index,
{
    #[inline]
    fn as_value(&self) -> Option<&T> {
        if let SlotState::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn as_value_mut(&mut self) -> Option<&mut T> {
        if let SlotState::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn into_value(self) -> Option<T> {
        if let SlotState::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn next_empty(&self) -> Option<I> {
        if let SlotState::Empty(next) = self {
            *next
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct Slot<T, I, G> {
    gen: G,
    state: SlotState<T, I>,
}

impl<T, I, G> Slot<T, I, G>
where
    G: Gen,
{
    #[inline]
    const fn new() -> Self {
        Self {
            gen: G::FIRST,
            state: SlotState::Empty(None),
        }
    }
}

pub struct Generational<I, G>(PhantomData<(I, G)>);
// SAFETY: invariant is upheld
unsafe impl<I, G> StrategyKind for Generational<I, G>
where
    I: Index,
    G: Gen,
{
    type Id = Id<I, G>;
    type Strategy<T> = GenerationalStrat<T, I, G>;
}

#[derive(Clone)]
pub struct GenerationalStrat<T, I, G> {
    slots: Vec<Slot<T, I, G>>,
    occupied: I,
    empty_slots_head: Option<I>,
}

impl<T, I, G> std::fmt::Debug for GenerationalStrat<T, I, G>
where
    T: std::fmt::Debug,
    I: common::Index + std::fmt::Debug,
    G: Gen + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T, I, G> GenerationalStrat<T, I, G>
where
    I: Index,
    G: Gen,
{
    #[inline]
    fn get_slot(&self, index: I) -> Option<&Slot<T, I, G>> {
        self.slots.get(index.to_usize())
    }

    #[inline]
    fn get_slot_mut(&mut self, index: I) -> Option<&mut Slot<T, I, G>> {
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
        self.empty_slots_head = empty_slots_head.state.next_empty();

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

impl<T, I, G> Default for GenerationalStrat<T, I, G>
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
unsafe impl<T, I, G> Strategy<T> for GenerationalStrat<T, I, G>
where
    I: Index,
    G: Gen,
{
    type Kind = Generational<I, G>;

    #[inline(always)]
    fn len(&self) -> usize {
        self.occupied.to_usize()
    }

    #[inline(always)]
    fn get(&self, id: <Self::Kind as StrategyKind>::Id) -> Option<&T> {
        let slot = self.get_slot(id.index)?;
        if id.gen != slot.gen {
            return None;
        }

        slot.state.as_value()
    }

    #[inline(always)]
    fn get_mut(&mut self, id: <Self::Kind as StrategyKind>::Id) -> Option<&mut T> {
        let slot = self.get_slot_mut(id.index)?;
        if id.gen != slot.gen {
            return None;
        }

        slot.state.as_value_mut()
    }

    fn insert(&mut self, value: T) -> <Self::Kind as StrategyKind>::Id {
        self.occupied = self
            .occupied
            .next()
            .unwrap_or_else(|| panic!("exceeded maximum number of elements in the stadium"));
        let index = self.acquire_slot();

        // SAFETY: `acquire_slot` guarantees a slot exists at the given index and that it is not a
        // tombstone
        let slot = unsafe { self.get_slot_mut(index).unwrap_unchecked() };
        slot.state = SlotState::Occupied(value);

        Id {
            index,
            gen: slot.gen,
        }
    }

    fn iter<'this>(
        &'this self,
    ) -> impl Iterator<Item = (<Self::Kind as StrategyKind>::Id, &'this T)>
    where
        T: 'this,
    {
        self.slots.iter().enumerate().filter_map(|(index, slot)| {
            slot.state.as_value().map(|v| {
                (
                    Id {
                        index: I::from_usize(index),
                        gen: slot.gen,
                    },
                    v,
                )
            })
        })
    }

    fn iter_mut<'this>(
        &'this mut self,
    ) -> impl Iterator<Item = (<Self::Kind as StrategyKind>::Id, &'this mut T)>
    where
        T: 'this,
    {
        self.slots
            .iter_mut()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.state.as_value_mut().map(|v| {
                    (
                        Id {
                            index: I::from_usize(index),
                            gen: slot.gen,
                        },
                        v,
                    )
                })
            })
    }
}

impl<T, I, G> StrategyExtRemove<T> for GenerationalStrat<T, I, G>
where
    I: Index,
    G: Gen,
{
    fn remove(&mut self, id: <Self::Kind as StrategyKind>::Id) -> Option<T> {
        self.occupied = self.occupied.previous()?;
        let empty_slots_head = std::mem::replace(&mut self.empty_slots_head, Some(id.index));

        let slot = self.get_slot_mut(id.index)?;
        if (id.gen != slot.gen) || slot.state.as_value().is_none() {
            self.occupied = self.occupied.next().unwrap();
            self.empty_slots_head = empty_slots_head;
            return None;
        }

        // ok - slot contains a value
        let new_gen = slot.gen.next().expect("not exhausted");
        let old_state = std::mem::replace(&mut slot.state, SlotState::Empty(empty_slots_head));
        slot.gen = new_gen;

        // if there's no next gen for this slot, don't put it in the empty stack
        if new_gen.next().is_none() {
            self.empty_slots_head = empty_slots_head;
        }

        // SAFETY: the slot was already checked to be non-empty
        unsafe { Some(old_state.into_value().unwrap_unchecked()) }
    }
}

impl<T, I, G> StrategyExtClear<T> for GenerationalStrat<T, I, G>
where
    I: Index,
    G: Gen,
{
    fn clear(&mut self) {
        for index in 0..self.slots.len() {
            let index = I::from_usize(index);
            let slot = self.get_slot(index).expect("in bounds");

            self.remove(Id {
                index,
                gen: slot.gen,
            });
        }
    }
}

impl<T, I, G> StrategyExtMap<T> for GenerationalStrat<T, I, G>
where
    I: Index,
    G: Gen,
{
    #[inline(always)]
    fn map<U, F>(self, mut f: F) -> <Self::Kind as StrategyKind>::Strategy<U>
    where
        F: FnMut(T) -> U,
    {
        let slots = map_vec(self.slots, |slot| {
            let state = match slot.state {
                SlotState::Empty(next) => SlotState::Empty(next),
                SlotState::Occupied(value) => SlotState::Occupied(f(value)),
            };

            Slot {
                gen: slot.gen,
                state,
            }
        });

        GenerationalStrat {
            slots,
            occupied: self.occupied,
            empty_slots_head: self.empty_slots_head,
        }
    }
}

#[cfg(test)]
mod test {
    use super::Generational;
    use crate::{Id, Stadium as RawStadium};

    type Stadium<T> = RawStadium<T, Generational<u8, u8>>;

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
    fn test_insert_remove_insert() {
        let mut stadium = Stadium::new();
        let a = stadium.insert("hi");
        let _ = stadium.insert("hello");

        assert_eq!(stadium.remove(a), Some("hi"));

        let c = stadium.insert("hi again");

        assert_eq!(stadium.remove(a), None);
        assert_eq!(stadium.remove(c), Some("hi again"));
    }

    #[test]
    fn test_tombstone() {
        let mut stadium = Stadium::new();
        for _ in 0..u8::MAX {
            let a = stadium.insert("hi");
            assert_eq!(a.inner.index, 0);
            assert_eq!(stadium.remove(a), Some("hi"));
        }

        let a = stadium.insert("hi");
        assert_eq!(a.inner.index, 1);
        assert_eq!(stadium.remove(a), Some("hi"));
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

    #[test]
    fn test_map() {
        let mut stadium = Stadium::new();
        let a = stadium.insert(Some(false));
        let b = stadium.insert(Some(true));

        let stadium = stadium.map(|x| x.unwrap());
        let a = Id::map(a);
        let b = Id::map(b);

        assert!(!stadium[a]);
        assert!(stadium[b]);
    }
}
