# cl-safe-yaml

`cl-safe-yaml` is a deliberately restricted YAML parser for data-only
configuration. It converts text into ordinary Common Lisp hash tables, vectors,
strings, numbers, booleans, and the symbol `null`.

## Why use it?

Full YAML is a large language with tags, aliases, merge keys, and application
defined construction. Many services only need the JSON-shaped part of the
language but still want indentation, comments, readable strings, and block
scalars.

This library makes that narrower contract explicit. It supports:

- block and flow mappings and sequences;
- plain, single-quoted, double-quoted, literal, and folded scalars;
- common YAML 1.1 boolean, null, integer, hexadecimal, octal, and floating-point
  resolution;
- comments and document markers;
- duplicate-key reporting with source line numbers; and
- a fixed nesting limit for hostile input.

It rejects anchors, aliases, tags, merge keys, directives, executable object
construction, malformed indentation, and unsupported syntax. It is not intended
to be a complete YAML implementation; choosing it means choosing this smaller
input language.

## Why not cl-yaml?

The usual Common Lisp choice is [cl-yaml](https://github.com/eudoxia0/cl-yaml),
which is built on [cl-libyaml](https://github.com/eudoxia0/cl-libyaml), a CFFI
binding to the C `libyaml`. It is a full-spec parser and emitter, and it is the
right tool when you control the input or genuinely need all of YAML. For
*untrusted* configuration it is the wrong shape, for two reasons that are
properties of full YAML, not bugs in any one library:

- **Tags construct objects.** YAML tags (`!!type`, custom `!foo`) tell a parser
  to build arbitrary types from the document. This is the class behind
  SnakeYAML's CVE-2022-1471, PyYAML's `yaml.load` (the reason `safe_load`
  exists), and Ruby Psych. `cl-safe-yaml` rejects tags, so input can only ever
  become plain hash tables, vectors, strings, numbers, booleans, and `null` —
  never an instantiated object.
- **Anchors and aliases expand.** Nested aliases turn a few lines into gigabytes
  — the [billion laughs](https://en.wikipedia.org/wiki/Billion_laughs_attack)
  denial of service. `cl-safe-yaml` rejects anchors and aliases and bounds
  nesting, so there is nothing to expand.

Two further differences matter in practice. `cl-yaml` pulls in a native C
library through CFFI — something to install, build, and track for CVEs —
whereas `cl-safe-yaml` is pure Lisp. And where most parsers silently accept
duplicate mapping keys and keep one, `cl-safe-yaml` reports every duplicate with
its line number, so a document that says one thing to a validator and another to
a consumer cannot slip through unnoticed.

The pure-Lisp alternative, [nyaml](https://github.com/jasom/nyaml), avoids the C
dependency but is still a full-spec parser, so the tag and alias surface remains.
`cl-safe-yaml` is the only one of these that removes that surface by refusing to
parse it.

## Installation

Load the ASDF system:

```lisp
(asdf:load-system "cl-safe-yaml")
```

Dependencies are locked in `ocicl.csv`.

## Parse a document

```lisp
(multiple-value-bind (value duplicate-warnings)
    (cl-safe-yaml:parse-yaml
     "name: example\nports: [8080, 8443]\nenabled: true\n")
  (values (gethash "ports" value) duplicate-warnings))
```

`parse-yaml` reads the first document and returns two values: the parsed value
and duplicate-key descriptions in document order. A later duplicate wins, which
lets callers choose whether warnings should be fatal.

Malformed or unsupported input signals `yaml-parse-error`. Its
`yaml-parse-error-line` and `yaml-parse-error-message` readers provide structured
diagnostics.

## Run the tests and example

```sh
sbcl --noinform --non-interactive \
  --eval '(require "asdf")' \
  --eval '(asdf:load-asd (truename "cl-safe-yaml.asd"))' \
  --eval '(asdf:test-system "cl-safe-yaml")'

sbcl --noinform --script examples/basic.lisp
```

## Author

Anthony Green <green@moxielogic.com>

## License

MIT. See [LICENSE](LICENSE).
