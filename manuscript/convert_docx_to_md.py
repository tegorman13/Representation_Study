from docx import Document
from docx.oxml.ns import qn
from docx.document import Document as _Document
from docx.table import Table, _Cell
from docx.text.paragraph import Paragraph
import zipfile
import xml.etree.ElementTree as ET
import re

def iter_block_items(parent):
    """
    Yield each paragraph and table child within parent, in document order.
    Handles Documents, Tables, Paragraphs, and _Cells as parents.
    """
    if isinstance(parent, _Document):
        parent_element = parent.element.body
    elif isinstance(parent, Table):
        parent_element = parent._element
    elif isinstance(parent, Paragraph):
        parent_element = parent._element
    elif isinstance(parent, _Cell):
        parent_element = parent._element
    else:
        raise ValueError(f"Unsupported parent type: {type(parent)}")

    for child in parent_element.iterchildren():
        if child.tag == qn('w:p'):
            yield Paragraph(child, parent)
        elif child.tag == qn('w:tbl'):
            yield Table(child, parent)


def extract_comments(doc_path):
    """
    Extracts comments from the DOCX file.
    Returns a dictionary: {comment_id: (footnote_number, comment_text)}.
    """
    comment_map = {}
    footnote_counter = 1
    with zipfile.ZipFile(doc_path) as z:
        if 'word/comments.xml' in z.namelist():
            xml_content = z.read('word/comments.xml')
            tree = ET.XML(xml_content)
            ns = {'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
            for comment in tree.findall('.//w:comment', ns):
                comment_id = comment.attrib.get('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}id')
                texts = comment.findall('.//w:t', ns)
                comment_text = ''.join([t.text for t in texts if t.text]).strip()
                if comment_text:
                    comment_map[comment_id] = (footnote_counter, comment_text)
                    footnote_counter += 1
    return comment_map


def process_paragraph(para, comment_map, ns):
    """
    Processes a paragraph and inserts footnote markers for comments.
    """
    md = ""
    for run in para.runs:
        run_text = run.text
        comment_refs = run._r.findall('.//w:commentReference', ns)
        for cref in comment_refs:
            comment_id = cref.get(qn('w:id'))
            if comment_id in comment_map:
                footnote_number = comment_map[comment_id][0]
                run_text += f"[^{footnote_number}]"
        md += run_text
    return md


def process_table(table):
    """
    Converts a docx table to Markdown format, handling merged cells
    and multi-line text within cells correctly.
    """
    table_data = []
    for row in table.rows:
        row_data = []
        for cell in row.cells:
            # Recursively process block items within cells
            cell_text = ""
            for block in iter_block_items(cell):
                if isinstance(block, Paragraph):
                    # Newlines for paragraph breaks *within* a cell.
                    cell_text += block.text.strip() + "\n"
                elif isinstance(block, Table):
                    cell_text += process_table(block)  # Handle nested tables
            cell_text = re.sub(r'\s*\n\s*', '\n', cell_text).strip() # Clean extra whitespace
            # Escape | and \ characters in cell text
            cell_text = cell_text.replace('|', '\\|').replace('\\', '\\\\')
            row_data.append(cell_text)
        table_data.append(row_data)


    md = ""
    if table_data:
        # Construct the Markdown table
        header = table_data[0]
        md += "| " + " | ".join(header) + " |\n"
        md += "| " + " | ".join(["---"] * len(header)) + " |\n"
        for row in table_data[1:]:
            # Handle rows with fewer cells than the header (due to merging)
            row_str = "| " + " | ".join(row)
            if len(row) < len(header):
                row_str += " |" * (len(header) - len(row))  # Add empty cells
            row_str += " |\n"
            md += row_str

    return md


def convert_docx_to_md(doc_path):
    """
    Converts a DOCX file to Markdown, handling paragraphs, tables, and comments.
    """
    doc = Document(doc_path)
    ns = {'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
    comment_map = extract_comments(doc_path)
    md_text = ""

    for block in iter_block_items(doc):
        if isinstance(block, Paragraph):
            md_text += process_paragraph(block, comment_map, ns) + "\n\n"
        elif isinstance(block, Table):
            md_text += process_table(block) + "\n\n"

    if comment_map:
        md_text += "\n---\n\n"
        for comment_id, (num, text) in sorted(comment_map.items(), key=lambda x: x[1][0]):
            md_text += f"[^{num}]: {text}\n"
    return md_text


def save_markdown(output_path, content):
    """Saves the Markdown content to a file."""
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(content)


if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print("Usage: python convert_docx_to_md.py input.docx output.md")
        sys.exit(1)

    input_docx = sys.argv[1]
    output_md = sys.argv[2]

    md_content = convert_docx_to_md(input_docx)
    save_markdown(output_md, md_content)
    print(f"Markdown file saved as {output_md}")