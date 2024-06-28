use crate::{Gen, Id, Index};
use std::{cmp::Ordering, marker::PhantomData};

/// Seat of an [`AuxStadium`]>
#[derive(Debug)]
struct Seat<T, G> {
    gen: Option<G>,
    value: Option<T>,
}

impl<T, G> Seat<T, G> {
    fn empty() -> Self {
        Self {
            gen: None,
            value: None,
        }
    }
}

/// An auxiliary stadium is just like a stadium(super::Stadium), but instead of creating keys
/// itself, it allows you to associated the existing keys of a stadium(super::Stadium) with
/// other values.
///
/// Because of the way elements are assigned a seat, auxiliary stadiums might use as much memory as
/// the stadium(super::Stadium) generating the [`Id`]s, even with just one element. If you
/// believe only a few of your elements are going to need associated values, consider using a
/// [`HashMap`](std::collections::HashMap) instead. Note, however, that HashMaps will keep elements
/// associated to dead [`Id`]s indefinitely.
///
/// # Consistency
/// In order to avoid unpredictable behaviour, auxiliary stadiums should only operate on [`Id`]s
/// of elements that come from the same stadium(super::Stadium), called the parent, and that
/// aren't dead.
///
/// # How it Works
/// An auxiliary stadium is similar to a [`HashMap<Id<T>, U>`](std::collections::HashMap), except
/// that it stores the elements in a [`Vec`] of seats, just like an stadium(super::Stadium).
/// Each seat has a generation and can contain a value. This way, whenever an element is associated
/// with an [`Id`] of index `i`, it'll be stored in the seat at index `i`.
///
/// ## Insertion
/// Whenever an element is inserted into an auxiliary stadium, it will be assigned to the seat at
/// the given index of the [`Id`].
///
/// ## Removal
/// Whenever an element is removed from an auxiliary stadium, it is simply taken out of it's seat
/// and returned.
///
/// # Limitations
/// - The lifetime of elements associated with a dead [`Id`] is unspecified: they might be dropped
///   on a call to `insert` or only when the whole auxiliary stadium is dropped. If this behaviour is
///   undesired, manually remove elements from the auxiliary stadium before removing the element in
///   the parent stadium(super::Stadium).
#[derive(Debug)]
pub struct AuxStadium<T, U, I, G> {
    seats: Vec<Seat<U, G>>,
    occupied: I,
    _phantom_data: PhantomData<T>,
}

impl<T, U, I, G> AuxStadium<T, U, I, G>
where
    I: Index,
    G: Gen,
{
    pub fn new() -> Self {
        Self {
            seats: Vec::new(),
            occupied: I::from_usize(0),
            _phantom_data: PhantomData,
        }
    }

    /// Returns the number of elements in this stadium.
    #[inline]
    pub fn len(&self) -> usize {
        self.occupied.to_usize()
    }

    /// Whether this stadium is empty or not.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gets the seat at the given index.
    #[inline]
    fn get_seat(&self, index: I) -> Option<&Seat<U, G>> {
        self.seats.get(index.to_usize())
    }

    /// Gets the seat at the given index.
    #[inline]
    fn get_seat_mut(&mut self, index: I) -> Option<&mut Seat<U, G>> {
        self.seats.get_mut(index.to_usize())
    }

    /// Returns a reference to the element associated with the given [`Id`].
    #[inline]
    pub fn get(&self, id: Id<T, I, G>) -> Option<&U> {
        let seat = self.get_seat(id.index)?;
        if seat.gen.map(|gen| gen != id.gen).unwrap_or(true) {
            return None;
        }

        seat.value.as_ref()
    }

    /// Returns a mutable reference to the element associated with the given [`Id`].
    #[inline]
    pub fn get_mut(&mut self, id: Id<T, I, G>) -> Option<&mut U> {
        let seat = self.get_seat_mut(id.index)?;
        if seat.gen.map(|gen| gen != id.gen).unwrap_or(true) {
            return None;
        }

        seat.value.as_mut()
    }

    /// Associates a `value` with a given [`Id`].
    pub fn insert(&mut self, id: Id<T, I, G>, value: U) -> Option<U> {
        self.seats
            .resize_with(self.seats.len().max(id.index.to_usize() + 1), Seat::empty);

        let seat = &mut self.seats[id.index.to_usize()];
        let Some(seat_gen) = seat.gen else {
            debug_assert!(seat.value.is_none());
            seat.gen = Some(id.gen);
            seat.value = Some(value);
            return None;
        };

        match seat_gen.cmp(&id.gen) {
            Ordering::Greater => {
                if cfg!(debug_assertions) {
                    panic!("element being associated with dead id in auxiliary stadium");
                } else {
                    None
                }
            }
            Ordering::Less => {
                seat.gen = Some(id.gen);
                seat.value = Some(value);
                None
            }
            Ordering::Equal => {
                seat.gen = Some(id.gen);
                std::mem::replace(&mut seat.value, Some(value))
            }
        }
    }

    /// Removes a value with a given [`Id`] from this [`AuxStadium`].
    pub fn remove(&mut self, id: Id<T, I, G>) -> Option<U> {
        let seat = self.seats.get_mut(id.index.to_usize())?;
        if seat.gen != Some(id.gen) {
            return None;
        }

        seat.value.take()
    }

    /// Clears this [`AuxStadium`]. This is equivalent to [removing](Self::remove) every element, one
    /// by one.
    #[inline]
    pub fn clear(&mut self) {
        for seat in self.seats.iter_mut() {
            seat.value.take();
        }
    }

    /// Returns an iterator of `(Id, &Value)` tuples in this [`AuxStadium`].
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Id<T, I, G>, &U)> {
        self.seats.iter().enumerate().filter_map(|(index, seat)| {
            let gen = seat.gen?;
            seat.value
                .as_ref()
                .map(|v| (Id::new(I::from_usize(index), gen), v))
        })
    }

    /// Returns an iterator of `(Id, &mut Value)` tuples in this [`AuxStadium`].
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Id<T, I, G>, &mut U)> {
        self.seats
            .iter_mut()
            .enumerate()
            .filter_map(|(index, seat)| {
                let gen = seat.gen?;
                seat.value
                    .as_mut()
                    .map(|v| (Id::new(I::from_usize(index), gen), v))
            })
    }

    /// Returns an iterator of `(Id, Value)` tuples in this [`AuxStadium`].
    #[inline]
    pub fn drain(&mut self) -> impl Iterator<Item = (Id<T, I, G>, U)> + '_ {
        self.seats
            .iter_mut()
            .enumerate()
            .filter_map(|(index, seat)| {
                let gen = seat.gen?;
                seat.value
                    .take()
                    .map(|v| (Id::new(I::from_usize(index), gen), v))
            })
    }
}

impl<T, U, I, G> std::ops::Index<Id<T, I, G>> for AuxStadium<T, U, I, G>
where
    I: Index,
    G: Gen,
{
    type Output = U;

    #[inline]
    fn index(&self, id: Id<T, I, G>) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<T, U, I, G> std::ops::IndexMut<Id<T, I, G>> for AuxStadium<T, U, I, G>
where
    I: Index,
    G: Gen,
{
    #[inline]
    fn index_mut(&mut self, id: Id<T, I, G>) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}
