namespace PredictableCore.Markdown


inductive TextItem where
  | text : String → TextItem
  | bold : String → TextItem
  | italic : String → TextItem
  | strikethrough : String → TextItem
  | code : String → TextItem
  | link : (text : String) → (url : String) → TextItem

structure TableCell where
  content : List TextItem

structure TableItem (n : Nat) where
  headers : _root_.Vector String n
  rows : List (_root_.Vector TableCell n)

inductive MarkdownItem where
  | h1 : String → MarkdownItem
  | h2 : String → MarkdownItem
  | h3 : String → MarkdownItem
  | h4 : String → MarkdownItem
  | h5 : String → MarkdownItem
  | h6 : String → MarkdownItem
  | p : List TextItem → MarkdownItem
  | pre : (language : Option String) → (content : String) → MarkdownItem
  | img : (alt : String) → (url : String) → MarkdownItem
  | blockquote : List TextItem → MarkdownItem
  | ul : List MarkdownItem → MarkdownItem
  | ol : List MarkdownItem → MarkdownItem
  | li : String → MarkdownItem
  | hr : MarkdownItem
  | br : MarkdownItem
  | table : TableItem n → MarkdownItem


structure MarkdownTag where
  element : MarkdownItem
  children : List MarkdownItem := []

def renderTextItem : TextItem → String
  | .text s => s
  | .bold s => s!"**{s}**"
  | .italic s => s!"*{s}*"
  | .strikethrough s => s!"~~{s}~~"
  | .code s => s!"`{s}`"
  | .link text url => s!"[{text}]({url})"

def renderTextItems (items : List TextItem) : String :=
  items.map renderTextItem |> String.join

def renderTableCell (cell : TableCell) : String :=
  renderTextItems cell.content

def renderTableRow (row : Vector TableCell n) : String :=
  let cells := row.toList.map renderTableCell
  "| " ++ " | ".intercalate cells ++ " |"

def renderTableHeader (headers : Vector String n) : String :=
  let headerRow := "| " ++ " | ".intercalate headers.toList ++ " |"
  let separator := "| " ++ " | ".intercalate (headers.toList.map (fun _ => "---")) ++ " |"
  headerRow ++ "\n" ++ separator

def renderTable (table : TableItem n) : String :=
  let header := renderTableHeader table.headers
  let rows := table.rows.map renderTableRow
  header ++ "\n" ++ "\n".intercalate rows

def indent (level : Nat) : String :=
  "".pushn ' ' (level * 2)

mutual
  def renderMarkdownItemIndented (level : Nat) : MarkdownItem → String
    | .h1 s => s!"# {s}"
    | .h2 s => s!"## {s}"
    | .h3 s => s!"### {s}"
    | .h4 s => s!"#### {s}"
    | .h5 s => s!"##### {s}"
    | .h6 s => s!"###### {s}"
    | .p items => renderTextItems items
    | .pre lang content =>
      match lang with
      | some l => s!"```{l}\n{content}\n```"
      | none => s!"```\n{content}\n```"
    | .img alt url => s!"![{alt}]({url})"
    | .blockquote items => "> " ++ renderTextItems items
    | .ul items => renderUnorderedList level items
    | .ol items => renderOrderedList level 1 items
    | .li s => s
    | .hr => "---"
    | .br => ""
    | .table t => renderTable t

  def renderUnorderedList (level : Nat) : List MarkdownItem → String
    | [] => ""
    | [item] =>
        indent level
        ++ "- "
        ++ renderMarkdownItemIndented (level + 1) item
    | item :: rest =>
        indent level
        ++ "- "
        ++ renderMarkdownItemIndented (level + 1) item
        ++ "\n"
        ++ renderUnorderedList level rest

  def renderOrderedList (level : Nat) (n : Nat) : List MarkdownItem → String
    | [] => ""
    | [item] =>
        indent level
        ++ s!"{n}. "
        ++ renderMarkdownItemIndented (level + 1) item
    | item :: rest =>
        indent level
        ++ s!"{n}. "
        ++ renderMarkdownItemIndented (level + 1) item
        ++ "\n"
        ++ renderOrderedList level (n + 1) rest
end

def renderMarkdownItem : MarkdownItem → String :=
  renderMarkdownItemIndented 0

def renderMarkdownTag (tag : MarkdownTag) : String :=
  let element := renderMarkdownItem tag.element
  let children := tag.children.map renderMarkdownItem
  if children.isEmpty then
    element
  else
    element ++ "\n" ++ "\n".intercalate children

def renderMarkdown (md : List MarkdownTag) : String :=
  md.map renderMarkdownTag |> ("\n\n".intercalate ·)

class Markdown.Represent (α : Type) where
  toMarkdown : α → List MarkdownTag
