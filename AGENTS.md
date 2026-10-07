# Consumers

Verify a change against site-publisher and opentorah.
A design change should leave those consumers simpler.
Both `includeBuild` this tree when the checkout is at the default path.
Do not publish to test them.

`./gradlew` here is only this library.
`cd` does not carry to the next command, so put it on the same line.
Do not run a consumer task from this checkout.

```bash
cd ~/Podval/site-publisher && ./gradlew test
cd ~/OpenTorah/opentorah.org && ./gradlew :opentorah-core:test
```

Generate sites from site-publisher.
Pass an absolute `--target-directory-name` so generation does not replace the site's `_site`.
CI uses `--treat-errors-as-warnings`.

```bash
cd ~/Podval/site-publisher && ./gradlew run --args='/path/to/source --log-level=WARN --treat-errors-as-warnings --target-directory-name=/tmp/xml-verify/name'
```

- ~/OpenTorah/chumashquestions.org
- ~/OpenTorah/opentorah.org/docs
- ~/OpenTorah/alter-rebbe.org
- ~/Podval/dub.podval.org
- ~/Podval/www.podval.org
