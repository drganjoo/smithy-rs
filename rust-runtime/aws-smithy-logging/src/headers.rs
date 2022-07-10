/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
use std::fmt::{Debug, Error, Formatter};

use http::{header::HeaderName, HeaderMap};

use crate::{OrFmt, Sensitive};

/// Marks the sensitive data of a header pair.
#[derive(Default, Debug)]
pub struct HeaderMarker {
    /// Set to `true` to mark the value as sensitive.
    pub value: bool,
    /// Set to `Some(x)` to mark `key[x..]` as sensitive.
    pub key_suffix: Option<usize>,
}

pub(crate) fn noop_header_marker(_: &HeaderName) -> HeaderMarker {
    HeaderMarker::default()
}

/// A wrapper around [`&HeaderMap`](HeaderMap) which modifies the behavior of [`Debug`]. Closures are used to mark
/// mark specific parts of the `Uri`.
///
/// The [`Debug`] implementation will respect the `debug-logging` flag.
pub struct SensitiveHeaders<'a, F> {
    headers: &'a HeaderMap,
    marker: F,
}

impl<'a> SensitiveHeaders<'a, fn(&'a HeaderName) -> HeaderMarker> {
    /// Constructs a new [`SensitiveHeaders`] with nothing marked as sensitive.
    pub fn new(headers: &'a HeaderMap) -> Self {
        Self {
            headers,
            marker: noop_header_marker,
        }
    }
}

impl<'a, F> SensitiveHeaders<'a, F> {
    /// Marks specific header values and prefixed header keys as sensitive by supplying a closure
    /// over the header key/values pairs. The closure takes the form
    /// `Fn(&HeaderKey) -> HeaderMarker` where `HeaderMarker` marks the parts of the header pair
    /// which are sensitive.
    ///
    /// This accommodates the [httpPrefixHeaders trait] and [httpHeader trait].
    ///
    /// # Example
    ///
    /// ```
    /// # use aws_smithy_logging::{SensitiveHeaders, HeaderMarker};
    /// # use http::header::HeaderMap;
    /// # let headers = HeaderMap::new();
    /// // Headers with keys equal to "header-name" are sensitive
    /// let headers = SensitiveHeaders::new(&headers).mark(|key|
    ///     HeaderMarker {
    ///         value: key == "header-name",
    ///         key_suffix: None
    ///     }
    /// );
    /// println!("{headers:?}");
    /// ```
    ///
    /// [httpPrefixHeaders trait]: https://awslabs.github.io/smithy/1.0/spec/core/http-traits.html#httpprefixheaders-trait
    /// [httpHeader trait]: https://awslabs.github.io/smithy/1.0/spec/core/http-traits.html#httpheader-trait
    pub fn mark<G>(self, marker: G) -> SensitiveHeaders<'a, G> {
        SensitiveHeaders {
            headers: self.headers,
            marker,
        }
    }
}

/// Concatenates the [`Debug`] of [`&str`](str) and ['Sensitive<&str>`](Sensitive).
struct ThenDebug<'a>(&'a str, Sensitive<&'a str>);

impl<'a> Debug for ThenDebug<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "\"{}{}\"", self.0, self.1)
    }
}

impl<'a, F> Debug for SensitiveHeaders<'a, F>
where
    F: Fn(&'a HeaderName) -> HeaderMarker,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let iter = self.headers.iter().map(|(key, value)| {
            let HeaderMarker {
                value: value_sensitive,
                key_suffix,
            } = (self.marker)(key);

            let key = if let Some(key_suffix) = key_suffix {
                let key_str = key.as_str();
                OrFmt::Left(ThenDebug(
                    &key_str[..key_suffix],
                    Sensitive(&key_str[key_suffix..]),
                ))
            } else {
                OrFmt::Right(key)
            };

            let value = if value_sensitive {
                OrFmt::Left(Sensitive(value))
            } else {
                OrFmt::Right(value)
            };

            (key, value)
        });

        f.debug_map().entries(iter).finish()
    }
}

#[cfg(test)]
mod tests {
    use http::{header::HeaderName, HeaderMap, HeaderValue};

    use super::*;

    // This is needed because we header maps with "{redacted}" are disallowed.
    struct TestDebugMap([(&'static str, &'static str); 4]);

    impl Debug for TestDebugMap {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            f.debug_map().entries(self.0.into_iter()).finish()
        }
    }

    const HEADER_MAP: [(&str, &str); 4] = [
        ("name-a", "value-a"),
        ("name-b", "value-b"),
        ("prefix-a-x", "value-c"),
        ("prefix-b-y", "value-d"),
    ];

    fn to_header_map<I>(values: I) -> HeaderMap
    where
        I: IntoIterator<Item = (&'static str, &'static str)>,
    {
        values
            .into_iter()
            .map(|(key, value)| {
                (
                    HeaderName::from_static(key),
                    HeaderValue::from_static(value),
                )
            })
            .collect()
    }

    // "{\"name-a\": \"value-a\", \"name-b\": \"value-b\", \"prefix-a{redacted}\": \"value-c\", \"prefix-b-y\": \"value-d\"}"
    // "{\"name-a\": \"value-a\", \"name-b\": \"value-b\", \"prefix-a-{redacted}\": \"value-c\", \"prefix-b-y\": \"value-d\"}"

    #[test]
    fn mark_none() {
        let original: HeaderMap = to_header_map(HEADER_MAP);

        let output = SensitiveHeaders::new(&original);
        assert_eq!(format!("{:?}", output), format!("{:?}", original));
    }

    #[cfg(not(feature = "debug-logging"))]
    const ALL_VALUES_HEADER_MAP: [(&str, &str); 4] = [
        ("name-a", "{redacted}"),
        ("name-b", "{redacted}"),
        ("prefix-a-x", "{redacted}"),
        ("prefix-b-y", "{redacted}"),
    ];
    #[cfg(feature = "debug-logging")]
    const ALL_VALUES_HEADER_MAP: [(&str, &str); 4] = HEADER_MAP;

    #[test]
    fn mark_all_values() {
        let original: HeaderMap = to_header_map(HEADER_MAP);
        let expected = TestDebugMap(ALL_VALUES_HEADER_MAP);

        let output = SensitiveHeaders::new(&original).mark(|_| HeaderMarker {
            value: true,
            key_suffix: None,
        });
        assert_eq!(format!("{:?}", output), format!("{:?}", expected));
    }

    #[cfg(not(feature = "debug-logging"))]
    const NAME_A_HEADER_MAP: [(&str, &str); 4] = [
        ("name-a", "{redacted}"),
        ("name-b", "value-b"),
        ("prefix-a-x", "value-c"),
        ("prefix-b-y", "value-d"),
    ];
    #[cfg(feature = "debug-logging")]
    const NAME_A_HEADER_MAP: [(&str, &str); 4] = HEADER_MAP;

    #[test]
    fn mark_name_a_values() {
        let original: HeaderMap = to_header_map(HEADER_MAP);
        let expected = TestDebugMap(NAME_A_HEADER_MAP);

        let output = SensitiveHeaders::new(&original).mark(|name| HeaderMarker {
            value: name == "name-a",
            key_suffix: None,
        });
        assert_eq!(format!("{:?}", output), format!("{:?}", expected));
    }

    #[cfg(not(feature = "debug-logging"))]
    const PREFIX_A_HEADER_MAP: [(&str, &str); 4] = [
        ("name-a", "value-a"),
        ("name-b", "value-b"),
        ("prefix-a{redacted}", "value-c"),
        ("prefix-b-y", "value-d"),
    ];
    #[cfg(feature = "debug-logging")]
    const PREFIX_A_HEADER_MAP: [(&str, &str); 4] = HEADER_MAP;

    #[test]
    fn mark_prefix_a_values() {
        let original: HeaderMap = to_header_map(HEADER_MAP);
        let expected = TestDebugMap(PREFIX_A_HEADER_MAP);

        let prefix = "prefix-a";
        let output = SensitiveHeaders::new(&original).mark(|name: &HeaderName| HeaderMarker {
            value: false,
            key_suffix: if name.as_str().starts_with(prefix) {
                Some(prefix.len())
            } else {
                None
            },
        });
        assert_eq!(format!("{:?}", output), format!("{:?}", expected));
    }
}
