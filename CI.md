# Continuous Integration and Deployment (CI/CD)

This document describes how the build, release, and documentation processes are automated for **pyDatalog**.

---

## 1. Automated Builds (GitHub Actions)

We use GitHub Actions to automate the building of source distributions (`sdist`) and binary wheels. The configuration is defined in [build_wheels.yml](file:///home/pcarbonn/Documents/repos/pyDatalog/.github/workflows/build_wheels.yml).

### Running on Push and Pull Requests
Every time you push to any branch or open a pull request, GitHub Actions will:
1. **Make SDist**: Package the pure Python source code into a `.tar.gz` archive.
2. **Build Wheels**: Build C-extension optimized binary wheels (`.whl`) across three target platforms using `cibuildwheel`:
   - Linux (`ubuntu-latest`)
   - Windows (`windows-latest`)
   - macOS (`macos-latest`)

*Note: In these default pushes, packages are **not** published to PyPI.*

---

## 2. Releasing to PyPI (Push + Publish)

Publishing to PyPI is automated and occurs only when a GitHub Release is published.

### How to Publish a New Version:
1. **Update version number**: Increment the version key in `pyproject.toml`.
2. **Push changes**: Commit and push the updated files to the default branch.
3. **Draft a GitHub Release**:
   - Go to your repository on GitHub.
   - Under **Releases** on the right, click **Draft a new release**.
   - Create a new tag (e.g. `v0.17.5`) on publish.
   - Title the release and add a description of changes.
   - Click **Publish release**.

### The Publish Flow:
Once the release is published, the GitHub Actions workflow triggers the publish step:
- It downloads all the wheels and the source distribution.
- It automatically uploads these built binaries (wheels and `.tar.gz`) as assets to the **GitHub Release** page.
- It leverages **PyPI Trusted Publishing (OIDC)**, authentication is passwordless and secure.
- It uses `uv publish` to securely upload all compiled wheels and the source package to PyPI.

---

## 3. Documentation (Read the Docs)

The documentation is built with MkDocs and hosted on Read the Docs. The build environment is configured in [.readthedocs.yaml](file:///home/pcarbonn/Documents/repos/pyDatalog/.readthedocs.yaml).

Updates are triggered automatically via a GitHub webhook:
- **Pushes to default branch**: Triggers an automatic rebuild of the `latest` version of the documentation.
- **Git Tags (Releases)**: Triggers an automatic build of a new stable documentation version corresponding to the tag.
