Piano
======

> a musical instrument in which hammers, operated from a keyboard of ivories, strike its cast of strings

## Overview

This library provides parsing of ivory chord descriptor files and
functionality to lookup the times associated with an entity.

## Chord Descriptor Format

Unsorted, pipe-separated, entity and time.

```
<entity>|<time>
```

For example:

```
E+0072|2016-07-25
E+8680|2016-02-29
E+3787|2016-07-18
E+0190|2016-03-14
E+0022|2016-05-10
```

## API

This library is primarily designed to be consumed from C using the following API:

```c
//
// Lookup an entity in the chord descriptor.
//
// If the entity is found, its array of chord times are returned, sorted
// newest to oldest.
//
// If the entity is not found, the |out_count| will be set to 0.
//
error_t piano_lookup (
    piano_t *piano
  , size_t entity_size
  , const char *entity
  , int64_t *out_count
  , int64_t *out_times
  );
```
