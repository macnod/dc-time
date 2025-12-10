# dc-time

A small Common Lisp library for working with Unix timestamps, universal time, and formatted timestamps.

## Features

- Convert between Unix time (seconds since 1970) and universal time (seconds since 1900)
- Format timestamps as strings (e.g., `2023-10-05T14:30:45`)
- Measure elapsed time with high-resolution timers
- Current Unix time as integer

## Quick Start

```lisp
(ql:quickload :dc-time)
(use-package :dc-time)
```

## Usage Examples

### Get current Unix timestamp
```lisp
(current-unix-time)  ; => 1696519845
```

### Format current time
```lisp
(timestamp-string)  ; => "2023-10-05T14:30:45"
(timestamp-string :format "%Y-%M-%D %h:%m:%s")  ; => "2023-10-05 14:30:45"
```

### Measure elapsed time
```lisp
(defvar start (mark-time))
(sleep 1.5)
(elapsed-time start)  ; => 1.50234 (seconds)
```

### Convert time formats
```lisp
(universal-time-to-unix-time)  ; current Unix time
(unix-time-to-universal-time 1696519845)  ; convert back to universal time
```

## API Reference

| Function | Purpose |
|----------|---------|
| `current-unix-time` | Current Unix timestamp (integer) |
| `timestamp-string &key universal-time format` | Format time as string |
| `mark-time` | Start high-res timer (returns mark) |
| `elapsed-time start-time` | Seconds since `mark-time` |
| `universal-time-to-unix-time &optional time` | Universal → Unix time |
| `unix-time-to-universal-time &optional time` | Unix → Universal time |

**Constants:**
- `*unix-epoch*` - Universal time of Unix epoch (1970-01-01 00:00:00 UTC)

## Timestamp Format

`%Y-%M-%DT%h:%m:%s` → `2023-10-05T14:30:45`
- `%Y` = 4-digit year
- `%M` = 2-digit month
- `%D` = 2-digit day
- `%h` = 2-digit hour (24h)
- `%m` = 2-digit minute
- `%s` = 2-digit second

## Dependencies

- `dc-ds` (provides `ds` and `pick` utilities)

## License

MIT
