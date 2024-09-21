mod detail;

use crate::detail::Hand;

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Vec<&'a str> {
    let hands: Vec<_> = hands.iter().map(|&h| Hand::new(h).unwrap()).collect();
    match hands.iter().max() {
        Some(best) => hands.iter().filter(|&h| h == best).map(|h| h.hand_s()).collect(),
        None => Vec::new(),
    }
}
