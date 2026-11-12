# blogged

this is the resting place for my thoughts, please give them condolences if you come across, if this place ever changes again in the future please contact me for reverting this.  

# run commands

stack exec blogged watch --> test server
stack exec blogged build --> the default hakyll build
stack exec blogged rebuild --> rebuild 
rm -rf _site _cache --> full site cleanup (recommended when you have messed up)

# bookmarks

`data/curius-bookmarks.json` is the frozen historical archive. Linkr publishes a privacy-limited snapshot to
`data/linkr.json`, and `scripts/update_bookmarks.py` merges both sources into `data/bookmarks.json` during the
site build. Each Linkr snapshot commit triggers a deployment, and the midnight UTC schedule remains as a daily
fallback rebuild.
