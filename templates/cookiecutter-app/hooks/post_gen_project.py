import subprocess

project_name = "{{ cookiecutter.project_name }}"

print(f"Setting up {project_name}...")

try:
    print("  Creating virtual environment")
    subprocess.run(["uv", "venv"], check=True, capture_output=True)

    print("  Installing dependencies")
    subprocess.run(["uv", "add", "click", "rich"], check=True, capture_output=True)

    print("  Installing dev dependencies")
    subprocess.run(
        ["uv", "add", "--dev", "pytest", "pytest-mock", "ruff", "ty", "pytest-cov"],
        check=True,
        capture_output=True,
    )

    print("Done.")

except subprocess.CalledProcessError as e:
    print(f"\nError: {' '.join(e.cmd)} failed (exit {e.returncode})")
    if e.stdout:
        print(e.stdout.decode())
    if e.stderr:
        print(e.stderr.decode())
    exit(1)
