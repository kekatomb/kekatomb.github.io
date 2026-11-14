/-!
# `.env` files

Local builds can keep secrets (like the Last.fm API key) in a `.env` file in
the project folder, which git ignores. A real environment variable with the
same name always wins, so CI keeps using its secrets.

The format is the usual one: `NAME=value` per line; blank lines and `#`
comments are ignored; an optional `export ` prefix and matching quotes around
the value are stripped.
-/

namespace Site.Env

private def trimChars (cs : List Char) : List Char :=
  ((cs.dropWhile Char.isWhitespace).reverse.dropWhile Char.isWhitespace).reverse

/-- The `NAME=value` pairs in a `.env` file's text. -/
def parse (src : String) : List (String × String) :=
  (src.splitOn "\n").filterMap fun line =>
    let l := trimChars (line.toList.filter (· != '\r'))
    if l.isEmpty || l.head? == some '#' then none else
    let l := if "export ".toList.isPrefixOf l then trimChars (l.drop 7) else l
    let before := l.takeWhile (· != '=')
    let name := trimChars before
    if name.isEmpty || before.length == l.length then none else
    let value := trimChars (l.drop (before.length + 1))
    let value := match value with
      | q :: rest@(_ :: _) =>
        if (q == '"' || q == '\'') && rest.getLast? == some q then rest.dropLast else value
      | _ => value
    some (String.ofList name, String.ofList value)

/-- The value of `name`: from the environment if it's set there, otherwise from
the `.env` file's pairs. -/
def lookup (fileVars : List (String × String)) (name : String) : IO (Option String) := do
  if let some v ← IO.getEnv name then
    if !v.isEmpty then return some v
  return (fileVars.find? (·.1 == name)).map (·.2)

end Site.Env
