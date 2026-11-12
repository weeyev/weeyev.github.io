#!/usr/bin/env python3
"""Merge the frozen Curius archive with Linkr's public snapshot."""

from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urlsplit, urlunsplit


DEFAULT_ARCHIVE = Path("data/curius-bookmarks.json")
DEFAULT_LINKR = Path("data/linkr.json")
DEFAULT_OUTPUT = Path("data/bookmarks.json")

STATUS_DISPLAY = {
    "unread": (True, "#ef4444"),
    "done": (False, "#10b981"),
    "archived": (False, "#ef4444"),
}


def load_json(path: Path):
    try:
        with path.open(encoding="utf-8") as handle:
            return json.load(handle)
    except FileNotFoundError as error:
        raise ValueError(f"Missing required bookmark source: {path}") from error
    except json.JSONDecodeError as error:
        raise ValueError(f"Invalid JSON in {path}: {error}") from error


def normalize_url(url: str) -> str:
    """Normalize enough URL syntax to deduplicate Curius and Linkr records."""
    try:
        parsed = urlsplit(url.strip())
        scheme = parsed.scheme.lower()
        hostname = (parsed.hostname or "").lower()
        if not scheme or not hostname:
            return url.strip()

        port = parsed.port
        if port and not ((scheme == "http" and port == 80) or (scheme == "https" and port == 443)):
            hostname = f"{hostname}:{port}"
        path = parsed.path.rstrip("/") or "/"
        return urlunsplit((scheme, hostname, path, parsed.query, ""))
    except (TypeError, ValueError):
        return str(url or "").strip()


def extract_domain(url: str) -> str:
    try:
        return (urlsplit(url).hostname or "").removeprefix("www.")
    except (TypeError, ValueError):
        return ""


def parse_timestamp(value: str | None) -> datetime | None:
    if not value:
        return None
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
        if parsed.tzinfo is None:
            parsed = parsed.replace(tzinfo=timezone.utc)
        return parsed.astimezone(timezone.utc)
    except (TypeError, ValueError):
        return None


def parse_archive_date(value: str | None) -> datetime | None:
    if not value:
        return None
    try:
        return datetime.strptime(value, "%B %d, %Y").replace(tzinfo=timezone.utc)
    except (TypeError, ValueError):
        return None


def format_date(timestamp: str | None) -> str:
    parsed = parse_timestamp(timestamp)
    if parsed is None:
        return "Unknown date"
    return f"{parsed.strftime('%B')} {parsed.day}, {parsed.year}"


def validate_archive(data, source: Path) -> list[dict]:
    if not isinstance(data, list) or not all(isinstance(item, dict) for item in data):
        raise ValueError(f"{source} must contain a JSON array of bookmark objects")
    return data


def validate_linkr(data, source: Path) -> list[dict]:
    if not isinstance(data, dict) or data.get("version") != 1 or not isinstance(data.get("links"), list):
        raise ValueError(f"{source} must be a Linkr version 1 snapshot")

    links = data["links"]
    for index, link in enumerate(links):
        if not isinstance(link, dict):
            raise ValueError(f"{source} link {index} must be an object")
        if not isinstance(link.get("title"), str) or not isinstance(link.get("url"), str):
            raise ValueError(f"{source} link {index} is missing a title or URL")
        if link.get("status") not in STATUS_DISPLAY:
            raise ValueError(f"{source} link {index} has an unsupported status")
    return links


def linkr_bookmark(link: dict) -> dict:
    to_read, signal_color = STATUS_DISPLAY[link["status"]]
    timestamp = link.get("captured_at")
    return {
        "title": link["title"].strip() or link["url"],
        "url": link["url"],
        "date": format_date(timestamp),
        "domain": extract_domain(link["url"]),
        "snippet": "",
        "full_snippet": "",
        "to_read": to_read,
        "signal_color": signal_color,
        "has_snippet": False,
        "_sort_time": parse_timestamp(timestamp),
    }


def archive_bookmark(item: dict) -> dict:
    bookmark = dict(item)
    bookmark["signal_color"] = "#ef4444" if bookmark.get("to_read") else "#10b981"
    bookmark["_sort_time"] = parse_archive_date(bookmark.get("date"))
    return bookmark


def merge_bookmarks(archive: list[dict], linkr_links: list[dict]) -> list[dict]:
    merged = [archive_bookmark(item) for item in archive]
    positions = {
        normalize_url(item.get("url", "")): index
        for index, item in enumerate(merged)
        if item.get("url")
    }

    for link in linkr_links:
        key = normalize_url(link["url"])
        to_read, signal_color = STATUS_DISPLAY[link["status"]]
        if key in positions:
            existing = merged[positions[key]]
            existing["to_read"] = to_read
            existing["signal_color"] = signal_color
            continue

        positions[key] = len(merged)
        merged.append(linkr_bookmark(link))

    epoch = datetime.min.replace(tzinfo=timezone.utc)
    merged.sort(key=lambda item: item.get("_sort_time") or epoch, reverse=True)
    for item in merged:
        item.pop("_sort_time", None)
    return merged


def write_json(path: Path, data) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(f"{path.suffix}.tmp")
    with temporary.open("w", encoding="utf-8") as handle:
        json.dump(data, handle, indent=2, ensure_ascii=False)
        handle.write("\n")
    temporary.replace(path)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive", type=Path, default=DEFAULT_ARCHIVE)
    parser.add_argument("--linkr", type=Path, default=DEFAULT_LINKR)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    archive = validate_archive(load_json(args.archive), args.archive)
    linkr_links = validate_linkr(load_json(args.linkr), args.linkr)
    bookmarks = merge_bookmarks(archive, linkr_links)
    write_json(args.output, bookmarks)
    print(
        f"Merged {len(archive)} archived Curius bookmarks with "
        f"{len(linkr_links)} Linkr links into {len(bookmarks)} bookmarks."
    )


if __name__ == "__main__":
    main()
