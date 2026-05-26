## Changes in version 1.6.1

### Significant user-visible changes

* Move `gcloud` functions to `DEFUNCT` status as they have been fully migrated
  to `GCPtools`

### Bug fixes and minor improvements

* Improve formatting and spacing in source files and vignette for `BiocCheck`
* Add return value and example for `gcp-class` documentation

### Bug fixes and minor improvements

* Use `GCPtools::gcloud_exists` for internal checks
* Use `with_mocked_bindings` in tests

## Changes in version 1.4.0

### Significant user-visible changes

* Deprecate `gcloud` and `gsutil` utility functions in favor of `GCPtools`
  package
* Translate `nci-crdc` URIs to URLs in `drs_nci_crdc`

### New features

* Add `drs_nci_crdc` documentation and support in `drs_hub`

### Bug fixes and minor improvements

* Add `interactive()` condition to examples to avoid execution in
  non-interactive sessions
* Use exported functions from `GCPtools` directly
* Add `GCPtools` dependency
* Resolve `NOTE` from using unqualified `GET`
* Fix package name and other typos
* Remove `test_gcloud_sdk.R`

## Changes in version 1.2.0

### New features

* Remove defunct `drs_stat`, `drs_access_url`, and `drs_cp` functions
* Defunct `bucket_location` and `bucket` arguments

### Bug fixes and minor improvements

* Filter out "prerequisites" element from response (#103)
* Clean up unused `authenticate_*` code
* Remove `httr2` from `Suggests`

## Changes in version 1.0.0

* Initial Bioconductor submission.
