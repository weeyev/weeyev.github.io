import requests
import os

# Load .env manually
try:
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    env_file = os.path.join(base_dir, ".env")
    if os.path.exists(env_file):
        with open(env_file, "r") as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith("#") and "=" in line:
                    key, val = line.split("=", 1)
                    os.environ[key.strip()] = val.strip().strip("'").strip('"')
except Exception:
    pass

GEMINI_API_KEY = os.environ.get("GEMINI_API_KEY")
if not GEMINI_API_KEY:
    print("No API Key found")
    exit(1)

url = f"https://generativelanguage.googleapis.com/v1beta/models?key={GEMINI_API_KEY}"
response = requests.get(url)
if response.status_code == 200:
    models = response.json().get("models", [])
    for m in models:
        print(f"Name: {m['name']}")
        print(f"Supported methods: {m.get('supportedGenerationMethods', [])}")
        print("-" * 20)
else:
    print(f"Error: {response.status_code} {response.text}")
