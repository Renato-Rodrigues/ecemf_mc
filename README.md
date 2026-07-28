# ECEMF model comparison simplified charts

This repository includes code necessary to create model comparison charts for the working package 1 of the ECEMF project.

## Set the credentials for accessing the ECEMF Scenario Explorer database

This project uses the Python package [pyam](https://pyam-iamc.readthedocs.io) to query
scenario data directly from the IIASA database infrastructure.

Please run the following script once in a Python console:

```python
import pyam
pyam.iiasa.set_config("<username>", "<password>")
```

Refer to this [tutorial](https://pyam-iamc.readthedocs.io/en/stable/tutorials/iiasa_dbs.html)
for more information!

## LICENSE
This program is free software: you can redistribute it and/or modify it under the terms of the **GNU Affero General Public License** as published by the Free Software Foundation, **version 3** of the License or later. You can see the LICENSE details in https://www.gnu.org/licenses/agpl.txt


## AUTHOR

Renato Rodrigues, Robert Pietzcker
