<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.time

<!-- Note: this is an automatically generated file. Do not edit. -->

## hydra.core.time.Timespec

*(undocumented)*

`∀.record{seconds:int64@{description="Whole seconds since the Unix Epoch; signed, so instants in the far past or distant future are representable"}, nanoseconds:uint32@{description="Nanoseconds within the second, in the range [0, 999999999]; unsigned, as the value is never negative"}}@{description="The POSIX struct timespec, with the same semantics: an instant in time as a number of seconds and nanoseconds since the Unix Epoch (1970-01-01T00:00:00Z). The actual resolution is implementation- and filesystem-defined. See <time.h> (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/time.h.html) and XBD section 4.19, \"Seconds Since the Epoch\" (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap04.html#tag_04_19)."}`
