/-!
# Small text helpers

Lean's `String` API returns `String.Slice` for many operations, so the
generator works on `List Char` and converts back with `String.ofList`.
-/

namespace Site

def isSpace (c : Char) : Bool := c == ' ' || c == '\t'

def trimLeft (cs : List Char) : List Char := cs.dropWhile isSpace

def trimRight (cs : List Char) : List Char := (cs.reverse.dropWhile isSpace).reverse

def trim (cs : List Char) : List Char := trimRight (trimLeft cs)

def isBlank (cs : List Char) : Bool := (trim cs).isEmpty

def strTrim (s : String) : String := String.ofList (trim s.toList)

/-- Split text into lines, dropping carriage returns. -/
def toLines (s : String) : List (List Char) :=
  let clean := String.ofList (s.toList.filter (· != '\r'))
  (clean.splitOn "\n").map String.toList

def joinLines (ls : List (List Char)) : List Char :=
  match ls with
  | [] => []
  | l :: rest => rest.foldl (fun acc x => acc ++ '\n' :: x) l

def padLeft (n : Nat) (s : String) : String :=
  String.ofList (List.replicate (n - s.length) '0') ++ s

end Site
