## How-To: Run Experiments with Prolog and Python Integration

This script demonstrates how to integrate Python with Prolog using the `swiplserver` library, which provides an interface to SWI-Prolog. The main goal is to load application and infrastructure configurations into Prolog and query for a placement result.

### Prerequisites
- Install [SWI-Prolog](https://www.swi-prolog.org/Download.html).
- Install the `swiplserver` Python package: 
  ```bash
  pip install swiplserver
  ```
- Clone the repository.

---

### How to Use the Script

1. **Purpose**:
   The script takes two input arguments: an application Prolog file and an infrastructure Prolog file. It queries the Prolog knowledge base for a `placement/2` predicate.

2. **Arguments**:
   Run the script using the following arguments:
   - `-app`: Path to the application Prolog file.
   - `-infra`: Path to the infrastructure Prolog file.
   
   Example:
   ```bash
   python main.py -app input/application.pl -infra input/infrastructure.pl
   ```

3. **Output**:
   - If no placement is found, outputs:
     ```
     No placement found
     ```
   - If a placement is found, prints:
     ```
     Found placement for '<Application>'
     ```
     Followed by a structured representation of the placement details.

---

### Limitations
- This program does not support the continuous reasoning behaviour of FogBrainX