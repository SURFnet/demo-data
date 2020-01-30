# SURF DemoData toolkit

Can generate and export relational data for demo purposes

## Requirements

- Get [leiningen](https://leiningen.org/)
- Clone this repo
- Install locally as a jar:
  `lein install`
- Create standalone jar for command line use (in target directory):
  `lein uberjar`

## Use standalone (uberjar)

```
java -jar target/demo-data-standalone.jar bootstrap \
  path-to-swagger.json demodata-schema.json demodata-population.json
```

```
java -jar target/demo-data-standalone.jar generate \
  demodata-schema.json demodata-population.json output.json
```


## License

Copyright (C) 2020 SURFnet B.V.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see http://www.gnu.org/licenses/.

