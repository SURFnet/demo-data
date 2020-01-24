# SURF DemoData toolkit

Can generate and export relational data for demo purposes

# Requirements

  - Get [leiningen](https://leiningen.org/)
  - Clone this repo
  - Install locally as a jar:
    `lein install`
  - Create standalone jar for command line use (in target directory):
    `line uberjar`

# Use standalone (uberjar)

```
   java -jar target/demo-data-0.1.0-SNAPSHOT.jar bootstrap \
     path-to-swagger.json demodata-schema.json
```

```
   java -jar target/demo-data-0.1.0-SNAPSHOT.jar generate \
     demodata-schema.json demodata-population.json output.json
```

