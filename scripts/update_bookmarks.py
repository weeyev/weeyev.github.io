import requests
import json
from datetime import datetime
import os

API_URL = "https://curius.app/api/users/6447/searchLinks"
OUTPUT_FILE = "data/bookmarks.json"


def extract_domain(url):
    try:
        parsed = urlparse(url)
        domain = parsed.netloc
        return domain.replace("www.", "")
    except:
        return ""


def format_date(date_str):
    try:
        dt = datetime.fromisoformat(date_str.replace("Z", "+00:00"))
        return dt.strftime("%B %-d, %Y")
    except:
        return date_str


def truncate_snippet(text, limit=200):
    if not text:
        return ""
    if len(text) <= limit:
        return text
    return text[:limit].rsplit(" ", 1)[0] + "..."


def main():
    print(f"Fetching bookmarks from {API_URL}...")
    try:
        response = requests.get(API_URL)
        response.raise_for_status()
        data = response.json()
    except Exception as e:
        print(f"Error fetching data: {e}")
        return

    links = data.get("links", [])
    print(f"Found {len(links)} links.")

    processed_links = []

    for link in links:
        url = link.get("link", "#")
        title = link.get("title", "Untitled")
        created_date = link.get("createdDate", "")
        snippet = link.get("snippet", "")
        to_read = link.get("toRead")

        # Process fields
        domain = extract_domain(url)
        date_display = format_date(created_date)
        processed_snippet = truncate_snippet(snippet)

        # Determine signal color
        signal_color = "#10b981" if to_read is not None else "#ef4444"

        item = {
            "title": title,
            "url": url,
            "date": date_display,
            "domain": domain,
            "snippet": processed_snippet,
            "full_snippet": snippet,
            "to_read": bool(to_read),
            "signal_color": signal_color,
            "has_snippet": bool(snippet),
        }
        processed_links.append(item)

    # Ensure directory exists
    os.makedirs(os.path.dirname(OUTPUT_FILE), exist_ok=True)

    with open(OUTPUT_FILE, "w") as f:
        json.dump(processed_links, f, indent=2)

    print(f"Successfully saved {len(processed_links)} bookmarks to {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
