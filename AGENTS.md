## Downstream

This library is used by two of my projects:

- opentorah (https://github.com/opentorah/opentorah; locally - ~/OpenTorah/opentorah.org)
- site publisher (https://github.com/dubinsky/site-publisher; locally - ~/Podval/site-publisher)

Changes to this library need to be verified against those two consumers; design improvements to this library should make code in those two consumers cleaner.

Changes to this library also need to be verified against the behaviour of the site publisher when generating the sites that use it:

- ~/OpenTorah/chumashquestions.org
- ~/OpenTorah/opentorah.org/docs
- ~/OpenTorah/alter-rebbe.org
- ~/Podval/dub.podval.org
- ~/Podval/www.podval.org

## Consumer Gradle

`./gradlew` in this checkout is only this library. The shell cwd is this repo; there is no Gradle `cwd` parameter. After `./gradlew test` here, consumer checks are a **different Gradle root** — `cd` first. Do not run `:opentorah-core:test` (or any consumer task) from here.

Both consumers `includeBuild` this tree when present (override with `-PxmlDir=` only if the checkout is not at the default). Unreleased xml is picked up automatically; do not publish just to test them.

```bash
# this library
./gradlew test

# site-publisher (includeBuild default ../xml)
cd ~/Podval/site-publisher && ./gradlew test

# opentorah core (includeBuild default ../../Podval/xml)
cd ~/OpenTorah/opentorah.org && ./gradlew :opentorah-core:test
```

Generate sites from **site-publisher**, not from this repo. CI uses `--treat-errors-as-warnings`. Pass an absolute `--target-directory-name=` so generation does not wipe the site's `_site`.

```bash
cd ~/Podval/site-publisher && ./gradlew run --args='/path/to/source --log-level=WARN --treat-errors-as-warnings --target-directory-name=/tmp/xml-verify/name'
```
