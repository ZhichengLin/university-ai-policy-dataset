#!/usr/bin/env python3
"""
Generate full-text Markdown mirrors for policy source files.

Inputs (read-only):
  - 00_source_material/01_chinese_original
  - 00_source_material/02_international_original
  - 00_source_material/03_translations

Outputs (derived):
  - 00_source_material/04_policy_markdown/<source_subdir>/*.md
  - 00_source_material/04_policy_markdown/policy_markdown_index.csv
"""

from __future__ import annotations

import csv
import hashlib
import re
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

from bs4 import BeautifulSoup
from PyPDF2 import PdfReader


BLOCK_TAGS = {
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "p",
    "li",
    "blockquote",
    "pre",
}

HTML_SUFFIXES = {".html", ".htm", ".mhtml"}
PDF_SUFFIXES = {".pdf"}
SKIP_FILES = {".DS_Store"}


@dataclass
class MetaRow:
    dataset: str
    row_id: str
    institution: str
    source_url: str
    snapshot_filename: str
    release_date: str
    policy_scope: str


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def norm_space(text: str) -> str:
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    text = re.sub(r"[ \t]+", " ", text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()


def try_decode_html(raw: bytes) -> str:
    for enc in ("utf-8", "utf-8-sig", "gb18030", "gbk", "big5", "latin-1"):
        try:
            return raw.decode(enc)
        except UnicodeDecodeError:
            continue
    return raw.decode("utf-8", errors="ignore")


def load_metadata(root: Path) -> Tuple[Dict[str, MetaRow], List[MetaRow]]:
    snapshot_map: Dict[str, MetaRow] = {}
    s1_rows: List[MetaRow] = []

    specs = [
        (
            root / "20_coding/21_store/S1_china_analysis.csv",
            "S1",
            "id",
            "univ_name_cn",
        ),
        (
            root / "20_coding/21_store/S2_international_analysis.csv",
            "S2",
            "id",
            "univ_name",
        ),
    ]

    for path, dataset, id_col, inst_col in specs:
        if not path.exists():
            continue
        with path.open("r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            for row in reader:
                snapshot = (row.get("snapshot_filename") or "").strip()
                meta = MetaRow(
                    dataset=dataset,
                    row_id=(row.get(id_col) or "").strip(),
                    institution=(row.get(inst_col) or "").strip(),
                    source_url=(row.get("source_url") or "").strip(),
                    snapshot_filename=snapshot,
                    release_date=(row.get("release_date") or "").strip(),
                    policy_scope=(row.get("policy_scope") or "").strip(),
                )
                if dataset == "S1":
                    s1_rows.append(meta)
                if snapshot and snapshot not in snapshot_map:
                    snapshot_map[snapshot] = meta

    return snapshot_map, s1_rows


def fuzzy_match_chinese(file_name: str, s1_rows: List[MetaRow]) -> Optional[MetaRow]:
    hits = []
    for r in s1_rows:
        name = r.institution
        if not name:
            continue
        if name in file_name:
            hits.append(r)
    if len(hits) == 1:
        return hits[0]
    return None


def find_main_html_container(soup: BeautifulSoup) -> Tuple[object, str]:
    for tag in soup(["script", "style", "noscript", "svg", "canvas", "iframe", "footer", "header", "nav", "aside"]):
        tag.decompose()

    def score_element(el: object) -> Tuple[float, int]:
        text = norm_space(el.get_text("\n"))
        text_len = len(text)
        if text_len < 40:
            return (-1.0, text_len)
        link_len = 0
        for a in el.find_all("a"):
            link_len += len(norm_space(a.get_text(" ")))
        link_ratio = link_len / max(text_len, 1)
        punct_bonus = min(len(re.findall(r"[。！？.!?;:]", text)), 40) * 4.0
        score = (text_len * (1.0 - link_ratio)) + punct_bonus - (120.0 * link_ratio)
        return (score, text_len)

    # Special-case common CMS policy containers that often hold core text.
    special_selectors = [
        ("#zoom", "special:id-zoom"),
        (".v_news_content", "special:class-v_news_content"),
        ("#article", "special:id-article"),
        (".article-content", "special:class-article-content"),
    ]
    for selector, method in special_selectors:
        el = soup.select_one(selector)
        if el is not None:
            txt = norm_space(el.get_text("\n"))
            if len(txt) >= 20:
                return el, method

    # First preference: semantic containers
    candidates = []
    for selector, method in (
        ("article", "semantic:article"),
        ("main", "semantic:main"),
        ("[role='main']", "semantic:role-main"),
    ):
        for el in soup.select(selector):
            score, text_len = score_element(el)
            if score > 0:
                candidates.append((score, text_len, el, method))

    # Second preference: id/class hints
    kw = re.compile(
        r"(content|article|post|entry|main|detail|news|policy|guideline|text|正文|通知|公告|zoom|articlecon)",
        re.IGNORECASE,
    )
    for el in soup.find_all(["div", "section"]):
        attrs = " ".join(
            [
                el.get("id", "") or "",
                " ".join(el.get("class", []) or []),
            ]
        )
        if kw.search(attrs):
            score, text_len = score_element(el)
            if score > 0:
                candidates.append((score, text_len, el, "heuristic:id-class"))

    if candidates:
        candidates.sort(key=lambda x: (x[0], x[1]), reverse=True)
        _, _, best, method = candidates[0]
        return best, method

    body = soup.body if soup.body is not None else soup
    return body, "fallback:body"


def html_to_markdown(path: Path) -> Tuple[str, str]:
    raw = path.read_bytes()
    html = try_decode_html(raw)
    soup = BeautifulSoup(html, "html.parser")
    container, method = find_main_html_container(soup)

    lines: List[str] = []
    for el in container.find_all(list(BLOCK_TAGS)):
        text = norm_space(el.get_text(" ", strip=True))
        if not text:
            continue
        tag = el.name.lower()
        if tag.startswith("h") and len(tag) == 2 and tag[1].isdigit():
            level = max(1, min(6, int(tag[1])))
            lines.append(f'{"#" * level} {text}')
        elif tag == "li":
            lines.append(f"- {text}")
        elif tag == "blockquote":
            lines.append(f"> {text}")
        elif tag == "pre":
            lines.append("```")
            lines.append(text)
            lines.append("```")
        else:
            lines.append(text)

    if not lines:
        plain = norm_space(container.get_text("\n"))
        lines = plain.split("\n")

    content = "\n\n".join([ln.strip() for ln in lines if ln.strip()])
    # Fallback if block extraction was too sparse
    plain_all = norm_space(container.get_text("\n"))
    if len(content) < 0.6 * len(plain_all):
        content = plain_all
        method = f"{method}+fallback:plain"
    return content, method


def pdf_to_markdown(path: Path) -> Tuple[str, str]:
    reader = PdfReader(str(path))
    pages: List[str] = []
    for i, page in enumerate(reader.pages, start=1):
        txt = norm_space(page.extract_text() or "")
        if txt:
            pages.append(f"### Page {i}\n\n{txt}")
        else:
            pages.append(f"### Page {i}\n\n[No extractable text on this page]")
    return "\n\n".join(pages).strip(), "pypdf2:page-text"


def iter_source_files(root: Path) -> Iterable[Path]:
    source_dirs = [
        root / "00_source_material/01_chinese_original",
        root / "00_source_material/02_international_original",
        root / "00_source_material/03_translations",
    ]
    for d in source_dirs:
        for p in sorted(d.iterdir()):
            if not p.is_file():
                continue
            if p.name in SKIP_FILES:
                continue
            suffix = p.suffix.lower()
            if suffix in HTML_SUFFIXES or suffix in PDF_SUFFIXES:
                yield p


def render_markdown(
    src: Path,
    src_rel: str,
    src_sha256: str,
    content: str,
    extraction_method: str,
    meta: Optional[MetaRow],
    companion_pdf_rel: Optional[str] = None,
    companion_pdf_sha256: Optional[str] = None,
) -> str:
    now = datetime.now(timezone.utc).isoformat()

    header = [
        f"# Policy Text: {src.stem}",
        "",
        "## Provenance",
        f"- Source file: `{src_rel}`",
        f"- Source SHA256: `{src_sha256}`",
        f"- Source type: `{src.suffix.lower().lstrip('.')}`",
        f"- Extracted at (UTC): `{now}`",
        f"- Extraction method: `{extraction_method}`",
    ]
    if companion_pdf_rel is not None:
        header.append(f"- Companion PDF used for text: `{companion_pdf_rel}`")
    if companion_pdf_sha256 is not None:
        header.append(f"- Companion PDF SHA256: `{companion_pdf_sha256}`")

    if meta is not None:
        header.extend(
            [
                f"- Dataset link: `{meta.dataset}`",
                f"- Dataset row id: `{meta.row_id}`",
                f"- Institution: `{meta.institution}`",
                f"- Source URL: `{meta.source_url}`",
                f"- Snapshot filename (dataset): `{meta.snapshot_filename}`",
                f"- Policy scope (dataset): `{meta.policy_scope}`",
                f"- Release date (dataset): `{meta.release_date}`",
            ]
        )

    header.extend(
        [
            "",
            "## Full Policy Text",
            "",
            content if content else "[No extractable text found]",
            "",
        ]
    )
    return "\n".join(header)


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    out_root = root / "00_source_material/04_policy_markdown"
    out_root.mkdir(parents=True, exist_ok=True)

    snapshot_map, s1_rows = load_metadata(root)

    index_rows: List[dict] = []

    for src in iter_source_files(root):
        src_rel = str(src.relative_to(root))
        source_subdir = src.parent.name
        out_dir = out_root / source_subdir
        out_dir.mkdir(parents=True, exist_ok=True)
        out_file = out_dir / f"{src.stem}.md"

        src_sha = sha256_file(src)
        meta = snapshot_map.get(src.name)
        if meta is None and source_subdir == "01_chinese_original":
            meta = fuzzy_match_chinese(src.name, s1_rows)

        status = "ok"
        err = ""
        extraction_method = ""
        content = ""
        companion_pdf_rel: Optional[str] = None
        companion_pdf_sha: Optional[str] = None

        try:
            suffix = src.suffix.lower()
            if suffix in HTML_SUFFIXES:
                content, extraction_method = html_to_markdown(src)
                companion_pdf = src.with_suffix(".pdf")
                if companion_pdf.exists():
                    should_use_pdf = (
                        len(content) < 800
                        or extraction_method.startswith("fallback:body")
                        or extraction_method.startswith("special:id-zoom")
                    )
                    if should_use_pdf:
                        content, _ = pdf_to_markdown(companion_pdf)
                        companion_pdf_rel = str(companion_pdf.relative_to(root))
                        companion_pdf_sha = sha256_file(companion_pdf)
                        extraction_method = f"{extraction_method}+companion_pdf_text"
            elif suffix in PDF_SUFFIXES:
                content, extraction_method = pdf_to_markdown(src)
            else:
                status = "skipped"
                extraction_method = "unsupported"
        except Exception as e:  # noqa: BLE001
            status = "error"
            err = str(e)
            extraction_method = "failed"
            content = ""

        md = render_markdown(
            src=src,
            src_rel=src_rel,
            src_sha256=src_sha,
            content=content,
            extraction_method=extraction_method,
            meta=meta,
            companion_pdf_rel=companion_pdf_rel,
            companion_pdf_sha256=companion_pdf_sha,
        )
        out_file.write_text(md, encoding="utf-8")

        row_status = status
        if status == "ok" and len(content) < 200:
            row_status = "warning_low_text"

        index_rows.append(
            {
                "source_relpath": src_rel,
                "output_relpath": str(out_file.relative_to(root)),
                "source_type": src.suffix.lower().lstrip("."),
                "source_sha256": src_sha,
                "status": row_status,
                "error": err,
                "extraction_method": extraction_method,
                "content_characters": len(content),
                "dataset_match": meta.dataset if meta else "",
                "dataset_row_id": meta.row_id if meta else "",
                "institution": meta.institution if meta else "",
                "source_url": meta.source_url if meta else "",
            }
        )

    index_path = out_root / "policy_markdown_index.csv"
    with index_path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "source_relpath",
                "output_relpath",
                "source_type",
                "source_sha256",
                "status",
                "error",
                "extraction_method",
                "content_characters",
                "dataset_match",
                "dataset_row_id",
                "institution",
                "source_url",
            ],
        )
        writer.writeheader()
        writer.writerows(index_rows)

    summary_path = out_root / "README.md"
    n_total = len(index_rows)
    n_ok = sum(1 for r in index_rows if r["status"] == "ok")
    n_err = sum(1 for r in index_rows if r["status"] == "error")
    summary = [
        "# Policy Markdown Mirror",
        "",
        "This folder contains derived full-text Markdown mirrors for policy source documents.",
        "",
        "## Summary",
        f"- Total processed files: {n_total}",
        f"- Successful extracts: {n_ok}",
        f"- Failed extracts: {n_err}",
        "",
        "## Notes",
        "- Originals in `00_source_material/01_chinese_original`, `02_international_original`, and `03_translations` are unchanged.",
        "- Each Markdown file contains provenance metadata and extracted full text.",
        "- Index file: `policy_markdown_index.csv`.",
        "",
    ]
    summary_path.write_text("\n".join(summary), encoding="utf-8")


if __name__ == "__main__":
    main()
