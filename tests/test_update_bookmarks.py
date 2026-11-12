import importlib.util
import unittest
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "scripts" / "update_bookmarks.py"
SPEC = importlib.util.spec_from_file_location("update_bookmarks", SCRIPT)
bookmarks = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(bookmarks)


class MergeBookmarksTest(unittest.TestCase):
    def test_preserves_archive_metadata_and_appends_new_linkr_links(self):
        archive = [
            {
                "title": "Archived title",
                "url": "https://example.com/already-saved#section",
                "date": "May 9, 2026",
                "domain": "example.com",
                "snippet": "Curius description",
                "full_snippet": "Curius description",
                "to_read": False,
                "signal_color": "#ef4444",
                "has_snippet": True,
            }
        ]
        linkr = [
            {
                "title": "Updated private title",
                "url": "https://example.com/already-saved",
                "captured_at": "2026-08-13T10:00:00Z",
                "status": "unread",
            },
            {
                "title": "New Linkr link",
                "url": "https://www.example.org/new",
                "captured_at": "2026-08-14T10:00:00Z",
                "status": "done",
            },
        ]

        merged = bookmarks.merge_bookmarks(archive, linkr)

        self.assertEqual(len(merged), 2)
        self.assertEqual(merged[0]["title"], "New Linkr link")
        self.assertEqual(merged[0]["domain"], "example.org")
        self.assertEqual(merged[1]["title"], "Archived title")
        self.assertEqual(merged[1]["snippet"], "Curius description")
        self.assertTrue(merged[1]["to_read"])
        self.assertEqual(merged[1]["signal_color"], "#ef4444")

    def test_colors_only_read_and_done_links_green(self):
        archived_read = bookmarks.archive_bookmark(
            {"title": "Read", "date": "May 9, 2026", "to_read": False, "signal_color": "#ef4444"}
        )
        archived_unread = bookmarks.archive_bookmark(
            {"title": "Unread", "date": "May 9, 2026", "to_read": True, "signal_color": "#10b981"}
        )

        self.assertEqual(archived_read["signal_color"], "#10b981")
        self.assertEqual(archived_unread["signal_color"], "#ef4444")
        self.assertEqual(bookmarks.linkr_bookmark({
            "title": "Done", "url": "https://example.com/done", "captured_at": None, "status": "done"
        })["signal_color"], "#10b981")
        self.assertEqual(bookmarks.linkr_bookmark({
            "title": "Unread", "url": "https://example.com/unread", "captured_at": None, "status": "unread"
        })["signal_color"], "#ef4444")
        self.assertEqual(bookmarks.linkr_bookmark({
            "title": "Archived", "url": "https://example.com/archived", "captured_at": None, "status": "archived"
        })["signal_color"], "#ef4444")

    def test_rejects_private_and_unknown_linkr_schema(self):
        with self.assertRaisesRegex(ValueError, "version 1"):
            bookmarks.validate_linkr({"links": []}, Path("linkr.json"))

        with self.assertRaisesRegex(ValueError, "unsupported status"):
            bookmarks.validate_linkr(
                {
                    "version": 1,
                    "links": [
                        {
                            "title": "Example",
                            "url": "https://example.com",
                            "captured_at": None,
                            "status": "private",
                        }
                    ],
                },
                Path("linkr.json"),
            )


if __name__ == "__main__":
    unittest.main()
