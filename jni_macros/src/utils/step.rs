use proc_macro2::{Span, TokenTree};
use syn::{buffer::Cursor, parse::ParseStream};

pub enum TokensMatchResult<'c> {
    /// The tokens matched a pattern.
    /// Contains a [`Cursor`] for the next token in the [`ParseStream`].
    Matched(Cursor<'c>),
    NotMatched,
    NoMoreTokens,
}

// pub trait TokenPattern {
//     fn matches<'c>(&self, first: &TokenTree, cursor: Cursor<'c>) -> TokensMatchResult<'c>;
// }
// impl<F> TokenPattern for F
// where F: for<'c> Fn(&TokenTree, Cursor<'c>) -> Option<Cursor<'c>> {
//     #[inline]
//     fn matches<'c>(&self, first: &TokenTree, cursor: Cursor<'c>) -> TokensMatchResult<'c> {
//         match self(first, cursor) {
//             Some(next) => TokensMatchResult::Matched(next),
//             None => TokensMatchResult::NotMatched,
//         }
//     }
// }
// /// Implement [`TokenPattern`] for a list of functions that match each token consecutively,
// /// requiring that all `F`s be matched.
// impl<F> TokenPattern for [F]
// where F: Fn(&TokenTree) -> bool {
//     /// Matches a pattern in a [`ParseStream`] where each consecutive token must match its respective pattern function `F`.
//     fn matches<'c>(&self, first: &TokenTree, cursor: Cursor<'c>) -> TokensMatchResult<'c> {
//         let mut pattern_cursor = CursorIter::new(cursor);
//         // The token that is currently being processed
//         let mut token = first.clone();
//         // Tells whether the following loop can continue checking if the pattern matches, or if should stop
//         let mut matching = true;
//
//         for predicate in self.into_iter() {
//             if !predicate(&token) {
//                 matching = false;
//                 break;
//             }
//             // Setup nenxt token
//             token = match pattern_cursor.next() {
//                 Some(token) => token,
//                 None => return TokensMatchResult::NoMoreTokens,
//             };
//         }
//
//         if matching {
//             // The pattern matched
//             TokensMatchResult::Matched(pattern_cursor.cursor)
//         } else {
//             TokensMatchResult::NotMatched
//         }
//     }
// }

/// Holds the return values for [`step_until()`].
pub struct StepResult {
    /// The tokens that precede the pattern found in the input stream.
    pub pre_tokens: Vec<TokenTree>,
    /// The tokens that were matched in the input stream.
    pub pattern_tokens: Vec<TokenTree>,
}

/// Parses an **input** [`ParseStream`] and looks for a **pattern** defined by a function `F`.
/// 
/// If some tokens are found that match the **pattern**,
/// the [`ParseStream`] will be *advanced* to the token *directly after* the last token in the pattern.
/// The [**return value**][StepResult] will contain the tokens that *matched the pattern*
/// and the tokens that were *skipped* while looking for the pattern.
/// 
/// In the case that no tokens matching the pattern were found,
/// an [`Error`][syn::Error] will be returned.
/// 
/// ## Pattern function
/// 
/// The **pattern** is a function that takes the *current token* and a [`Cursor`] containing the *next token*.
/// The *pattern function* steps through the tokens,
/// and if it finds a match it returns the [`Cursor`] for the next token after the pattern tokens.
/// If no match was found it will return [`None`].
pub fn step_until<F>(input: ParseStream<'_>, pattern: F) -> syn::Result<StepResult>
where F: for<'c> Fn(&TokenTree, Cursor<'c>) -> Option<Cursor<'c>> {
    step_until_impl(input, |first, next| {
        match pattern(first, next) {
            Some(next) => TokensMatchResult::Matched(next),
            None => TokensMatchResult::NotMatched,
        }
    })
}

/// Like [`step_until()`], but the pattern is a list of functions,
/// where each function must match each individual token *consecutively*.
/// This means that if the first element of **pattern** matches a token,
/// then the second element must match the *next token*, and so on.
/// 
/// `panic!s` if **pattern** is empty.
pub fn step_until_each<F>(input: ParseStream<'_>, pattern: impl AsRef<[F]>) -> syn::Result<StepResult>
where F: Fn(TokenTree) -> bool {
    let pattern = pattern.as_ref()
        .iter()
        .collect::<Box<[_]>>();

    step_until_impl(input, |first, mut next| {
        // The token that is currently being processed
        let mut token = first.clone();
        let mut iter = pattern.iter();
        let mut predicate = iter.next().expect("Pattern list can't be empty");

        loop {
            // Match the current token with the current function
            if !predicate(token) {
                return TokensMatchResult::NotMatched
            }
            // Check if there are more functions to match a token with
            // Only advance cursor if there are more functions to match with
            if let Some(f) = iter.next() {
                predicate = f;
                // Get the next token to match
                token = match next.token_tree() {
                    Some((token, cursor)) => {
                        next = cursor;
                        token
                    },
                    None => return TokensMatchResult::NoMoreTokens,
                };
            } else {
                break;
            }
        }

        // Finished matching the pattern
        TokensMatchResult::Matched(next)
    })
}

fn step_until_impl<M>(input: ParseStream<'_>, matcher: M) -> syn::Result<StepResult>
where M: for<'c> Fn(&TokenTree, Cursor<'c>) -> TokensMatchResult<'c> {
    // Tokens that were skipped while parsing until the pattern was found.
    // AKA these tokense precede the pattern.
    let mut pre_tokens = Vec::new();

    // The tokens that matched with the pattern
    let pattern_tokens = input.step(|cursor| {
        let mut main_cursor = CursorIter::new(*cursor);
        
        // Iterate through the TokenStream until the pattern is found
        while let Some(token) = main_cursor.next() {
            match matcher(&token, main_cursor.cursor) {
                TokensMatchResult::Matched(next) => {
                    // Collect the tokens between the current and returned cursors
                    // These are the tokens matched by the pattern
                    let mut pattern_tokens = Vec::new();
                    // Push current token, as it was matched.
                    pattern_tokens.push(token);
                    // Push tokens until it reaches the next token to be parsed
                    while let Some(token) = main_cursor.next() {
                        pattern_tokens.push(token);

                        if main_cursor.cursor == next {
                            break;
                        }
                    }

                    return Ok((pattern_tokens, next));
                },
                TokensMatchResult::NoMoreTokens => break,
                TokensMatchResult::NotMatched => pre_tokens.push(token),
            }
        }

        Err(syn::Error::new(Span::call_site(), "Invalid tokens; Pattern not found"))
    })?;

    Ok(StepResult { pre_tokens, pattern_tokens })
}


// pub fn step_until<'c>(input: ParseStream<'_>, pattern: impl TokenPattern) -> syn::Result<StepResult> {
//     // Tokens that were skipped while parsing until the pattern was found.
//     // AKA these tokense precede the pattern.
//     let mut pre_tokens = Vec::new();
//
//     // The tokens that matched with the pattern
//     let pattern_tokens = input.step(|cursor| {
//         let mut main_cursor = CursorIter::new(*cursor);
//         // Iterate through the TokenStream until the pattern is found
//         while let Some(token) = main_cursor.next() {
//             match pattern.matches(&token, main_cursor.current) {
//                 TokensMatchResult::Matched(next) => {
//                     // Collect the tokens between the current and returned cursors
//                     // These are the tokens matched by the pattern
//                     let mut pattern_tokens = Vec::new();
//                     // Push tokens until the returned cursor is reached
//                     while let Some(token) = main_cursor.next() {
//                         if main_cursor.current == next {
//                             break;
//                         }
//
//                         pattern_tokens.push(token);
//                     }
//
//                     return Ok((pattern_tokens, next));
//                 },
//                 TokensMatchResult::NoMoreTokens => break,
//                 TokensMatchResult::NotMatched => pre_tokens.push(token),
//             }
//         }
//
//         Err(syn::Error::new(Span::call_site(), "Invalid tokens; Did not find desired pattern"))
//     })?;
//
//     Ok(StepResult { pre_tokens, pattern_tokens })
// } 

/// Helper iterator for [`Cursor`].
/// This does not advance the original [`Cursor`],
/// that is not possible.
struct CursorIter<'a> {
    cursor: Cursor<'a>
}
impl<'a> CursorIter<'a> {
    pub fn new(cursor: Cursor<'a>) -> Self {
        Self { cursor }
    }
}
impl<'a> Iterator for CursorIter<'a> {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        let (token, next) = self.cursor.token_tree()?;
        self.cursor = next;
        Some(token)
    }
}
