Borel (OpenStack)
=================

This package provides basic primitives for metering and billing OpenStack tenancies. It leverages the following components:

  * Chevalier: an indexing source for Vaultaire. Borel finds the data sources and schemas associated with an OpenStack tenancy from this index.
  * Marquise: a data client for Vaultaire. Borel fetches the raw data via this client.
  * Ceilometer: a decoder/aggregator for OpenStack data. Borel feeds raw data from Marquise to Ceilometer.

## Dependencies

 - [ZeroMQ](http://zeromq.org/) (need v4.0 or above)
 - [chevalier-common](https://github.com/anchor/chevalier-common)
 - [marquise](https://github.com/anchor/marquise)
 - [vaultaire-common](https://github.com/anchor/vaultaire-common)
 - [ceilometer-common](https://github.com/anchor/ceilometer-common)

## Configuration

Example configuration:

```
origins             = [ "ABCDEF" ]

marquise-reader-uri = "tcp://example.com:999"
chevalier-uri       = "tcp://nsa.gov:3333"

flavors {
  asio {
    id   = "asio"
  }
  
  koolaid {
    id   = "koolaid"
  }
}
```

 - `origins`: Vaul taire data origins.
 - URIs: to the instances of Marquise/Chevalier
 - `flavors`: instance flavors supported in your OpenStack setup.
 
## Running

A simple example usage can be found in `src/Example.hs`. 