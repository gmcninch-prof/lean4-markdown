namespace Markdown


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

/-! ## Helper Functions for Common Markdown Patterns

These helpers reduce boilerplate when implementing `Markdown.Represent` instances.
Choose the appropriate helper based on your use case:

**Tables:**
- `textCell` - Create a simple text cell: `textCell "value"` instead of `{ content := [.text "value"] }`
- `tableWithFooter` - Table with a single footer string (e.g., "Total: 5 items")
- `tableWithFooterItems` - Table with multiple footer parts (e.g., counts + recommendation)
- `tableOrEmpty` - Table that shows an empty message when the list is empty

**Detail views:**
- `headerWithInfo` - h2 header followed by a bulleted info list
- `section3` - h3 header followed by any content
- `infoList` - Simple unordered list from strings
-/

/-- Create a simple text cell for tables.
Use this instead of the verbose `{ content := [.text s] }` pattern. -/
def textCell (s : String) : TableCell := { content := [.text s] }

/-- Create a table with a footer paragraph.
Use this for the common pattern of displaying data in a table with summary text below. -/
def tableWithFooter
    (headers : Vector String n)
    (rows : List (Vector TableCell n))
    (footerText : String)
    : List MarkdownTag :=
  let table := MarkdownItem.table { headers := headers, rows := rows }
  let footer := MarkdownItem.p [.text footerText]
  [{ element := table }, { element := footer }]

/-- Create a table with multiple footer text items (joined together).
Use this when the footer has multiple parts like counts and recommendations. -/
def tableWithFooterItems
    (headers : Vector String n)
    (rows : List (Vector TableCell n))
    (footerItems : List String)
    : List MarkdownTag :=
  let table := MarkdownItem.table { headers := headers, rows := rows }
  let footer := MarkdownItem.p (footerItems.map TextItem.text)
  [{ element := table }, { element := footer }]

/-- Render a table if items exist, otherwise show an empty message.
Use this for lists that may be empty. The footer function receives the full list
for computing summaries like counts. -/
def tableOrEmpty
    (items : List α)
    (emptyMessage : String)
    (headers : Vector String n)
    (toRow : α → Vector TableCell n)
    (mkFooter : List α → String)
    : List MarkdownTag :=
  if items.isEmpty then
    [{ element := MarkdownItem.p [.text emptyMessage] }]
  else
    let rows := items.map toRow
    let table := MarkdownItem.table { headers := headers, rows := rows }
    let footer := MarkdownItem.p [.text (mkFooter items)]
    [{ element := table }, { element := footer }]

/-- Create a header with an info list below it.
Use this for detail views with a title and bullet points. -/
def headerWithInfo (title : String) (items : List String) : List MarkdownTag :=
  let header := MarkdownItem.h2 title
  let info := MarkdownItem.ul (items.map MarkdownItem.li)
  [{ element := header }, { element := info }]

/-- Create a section with h3 header and content -/
def section3 (title : String) (content : MarkdownItem) : List MarkdownTag :=
  [{ element := MarkdownItem.h3 title }, { element := content }]

/-- Create a simple info list (unordered list of strings) -/
def infoList (items : List String) : MarkdownItem :=
  MarkdownItem.ul (items.map MarkdownItem.li)
