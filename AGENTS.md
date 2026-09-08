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

`./gradlew` in this checkout is **only this library**. The Grok shell cwd is this repo on every command; there is no Gradle `cwd` parameter, and `cd` does not persist to the next tool call.

**Trap:** after `./gradlew test` here, another `./gradlew test` is still this library (often `UP-TO-DATE` in well under a second). Consumer checks are a **different Gradle root**. `cd` in the **same** command as `./gradlew`. Do not run `:opentorah-core:test` (or any consumer task) from here — that project does not exist in this tree.

Both consumers `includeBuild` this tree when present (override with `-PxmlDir=` only if the checkout is not at the default). Unreleased xml is picked up automatically; do not publish just to test them.

```bash
# this library (cwd is already this repo)
./gradlew test

# site-publisher — must cd in this command; includeBuild default ../xml
cd ~/Podval/site-publisher && ./gradlew test

# opentorah core — must cd in this command; includeBuild default ../../Podval/xml
cd ~/OpenTorah/opentorah.org && ./gradlew :opentorah-core:test
```

Generate sites from **site-publisher**, not from this repo. CI uses `--treat-errors-as-warnings`. Pass an absolute `--target-directory-name=` so generation does not wipe the site's `_site`.

```bash
cd ~/Podval/site-publisher && ./gradlew run --args='/path/to/source --log-level=WARN --treat-errors-as-warnings --target-directory-name=/tmp/xml-verify/name'
```
