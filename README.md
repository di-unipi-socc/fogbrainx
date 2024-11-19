<p><img align="left"  src="https://raw.githubusercontent.com/di-unipi-socc/fogbrainx/main/img/logo.png" width="300"> </p>

<br></br>
_continuous reasoning for managing next-gen Cloud-IoT applications in continuity with the CI/CD pipeline_

FogBrainX methodology is described in the following article:

> [Stefano Forti](http://pages.di.unipi.it/forti), [Giuseppe Bisicchia](http://pages.di.unipi.it/bisicchia), [Antonio Brogi](http://pages.di.unipi.it/brogi)<br>
> [**Declarative Continuous Reasoning in the Cloud-IoT Continuum**](https://doi.org/10.1093/logcom/exab083), <br>	
> *Journal of Logic and Computation (2022)*

If you wish to reuse source code in this repo, please consider citing it.

## How-To

The `main.py` script demonstrates how to integrate Python with Prolog using the `swiplserver` library, which provides an interface to SWI-Prolog. The main goal is to load application and infrastructure configurations into Prolog and query for a placement result.

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
- The `main.py` script does not support the continuous reasoning behaviour of FogBrainX
