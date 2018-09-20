# Login to AWS account via Okta (SAML) from the command line

Logs in to [AWS ECR](https://aws.amazon.com/ecr/) at the same time. Populates `$HOME/.aws/credentials` and runs `docker login` with temporary ECR credentials.

[Releases](https://github.com/gilt/okta-aws-login/releases)


# Install

```bash
curl -L https://raw.githubusercontent.com/gilt/okta-aws-login/master/install | /bin/bash
```

# CLI

```bash
Login to AWS via Okta/SAML.

Usage: okta-aws-login [-v|--verbose] [-V|--version] [-l|--list-profiles]
                      [-u|--user ARG] [-p|--aws-profile ARG] [-r|--region ARG]
                      [-c|--config-file ARG] [-k|--keep-reloading] [-E|--no-ecr]
  Login to AWS via Okta/SAML (source: https://github.com/gilt/okta-aws-login)
  Default config file: "$HOME/.okta-aws-login.json" Example config
  JSON:
  {"saml":[{"org":"orgname","aws_profile":"my-aws-profile","okta_aws_account_id":"0oa1298hUiqWerSnBVpO"},{"default":true,"org":"orgname","aws_profile":"my-default-aws-profile","okta_aws_account_id":"0oa87GhDsxZaQw32571u"}]}

Available options:
  -h,--help                Show this help text
  -v,--verbose             Be verbose.
  -V,--version             Print version and exit.
  -l,--list-profiles       List available AWS profiles and exit.
  -u,--user ARG            User name.
  -p,--aws-profile ARG     AWS profile. Defaults to value of AWS_PROFILE env
                           var, then to default config entry.
  -r,--region ARG          AWS region. (default: us-east-1)
  -c,--config-file ARG     Use alternative config
                           file. (default: "$HOME/.okta-aws-login.json")
  -k,--keep-reloading      Keep reloading session token hourly (that's the max
                           TTL at the moment). This only works well on a trusted
                           network where you don't need MFA.
  -E,--no-ecr              Skip 'docker login' to associated ECR registry,
                           enabled by default, requires docker binary available
                           in the PATH.

Log in using default AWS profile, you'll be prompted for user name / password:

  $ okta-aws-login

Specify user name and keep reloading session:

  $ okta-aws-login --user my-okta-user-name --keep-reloading

Log in with more than one AWS profile:

  $ okta-aws-login --user my-okta-user-name --aws-profile my-aws-profile1 --aws-profile my-aws-profile2

Skip ECR login (if you don't care about docker and don't have it installed)

  $ okta-aws-login --no-ecr --user my-okta-user-name --aws-profile my-aws-profile1
````


You need a `$HOME/.okta-aws-login.json` config file with list of profiles and associated Okta account IDs, e.g.

```json
{
  "saml": [
    {
      "org": "orgname",
      "aws_profile": "my-aws-profile",
      "okta_aws_account_id": "0oa1298hUiqWerSnBVpO"
    },
    {
      "default": true,
      "org": "orgname",
      "aws_profile": "my-default-aws-profile",
      "okta_aws_account_id": "0oa87GhDsxZaQw32571u"
    }
  ]
}
```


# Development

To build it from source you need [haskell stack](https://docs.haskellstack.org/en/stable/README/).
While not strictly required, `Makefile` tries to compress generated binaries with [upx](https://upx.github.io/) (`brew install upx`).
Optionally you also need [hlint](https://github.com/ndmitchell/hlint) (`stack install hlint`).


Okta [API](http://developer.okta.com/docs/api/resources/authn.html).

Okta official CLI [inspiration](https://github.com/oktadeveloper/okta-aws-cli-assume-role)
