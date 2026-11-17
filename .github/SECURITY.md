# Security Policy

## Supported Versions

Only the latest release of `pyDatalog` running on actively supported Python versions receives security updates and bug fixes.

| Version | Supported          |
| ------- | ------------------ |
| 0.22.x  | :white_check_mark: |
| < 0.22  | :x:                |

Python versions supported: **Python >= 3.10**

---

## Reporting a Vulnerability

The maintainers of `pyDatalog` take security seriously. If you discover a security vulnerability, **please do not disclose it publicly** by creating a public GitHub issue, discussion, or pull request.

Instead, please report vulnerabilities responsibly using one of the following methods:

### Method 1: GitHub Private Vulnerability Reporting (Preferred)
Submit a report privately through the repository's security advisory portal:
**[Report a vulnerability](https://github.com/pcarbonn/pyDatalog/security/advisories/new)**

This allows us to collaborate with you privately in a dedicated workspace, test fixes in a private fork, and assign CVE credits upon publication.

### Method 2: Direct Email
If you are unable to use GitHub Private Vulnerability Reporting, you can email:
- **Pierre Carbonnelle**: `pierre.carbonnelle@gmail.com`

Please include:
- A descriptive title and summary of the vulnerability.
- Affected component(s) or functions.
- Step-by-step instructions or minimal proof-of-concept (PoC) to reproduce the issue.
- Potential impact and any suggested mitigations.

### Response Timeline
- We will acknowledge receipt of your vulnerability report within 48–72 hours.
- We will keep you informed of our progress throughout the validation, remediation, and advisory release process.

---

## Security Considerations & Threat Model

`pyDatalog` embeds Datalog logic programming into the Python runtime.
- Methods such as `pyDatalog.ask()` and `@pyDatalog.program` transform logic terms and execute expressions within the Python runtime.
- **Untrusted Input**: Never pass unsanitized, untrusted user inputs directly into `ask()` or dynamically synthesized logic rules, as this can allow arbitrary Python expression evaluation, analogous to SQL injection or unsafe `eval()` usage.
