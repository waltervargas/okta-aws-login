# Login to AWS account via Okta (SAML) from the command line

Logs in to [AWS ECR](https://aws.amazon.com/ecr/) at the same time. Populates `$HOME/.aws/credentials` and runs `docker login` with temporary ECR credentials.

[Releases](https://github.com/gilt/okta-aws-login/releases)


# Install

```bash
curl -L https://raw.githubusercontent.com/gilt/okta-aws-login/master/install | /bin/bash
```

Initially this tool needs to be configured, you need to know:
   - Okta "embed" link for your AWS account
   - Max session duration for the AWS roles you are allowed to assume via SAML

Your system administrators should be able to provide both of these.


Example configuration (assuming you are allowed to have 12h sessions):
```bash
$ okta-aws-login  --configure --okta-embed-link https://YOURSITE.okta.com/home/amazon_aws/xxxxxxxxxxxxxxxxxxxx/yyy --aws-profile production --default --ecr --session-duration 43200
```


# CLI

```bash
$ okta-aws-login -h

Login to AWS via Okta/SAML.

Usage: okta-aws-login ([--configure] [-p|--aws-profile ARG]
                      [-l|--okta-embed-link ARG] [-d|--default] [-r|--ecr]
                      [-c|--config-file ARG] [-s|--session-duration ARG] |
                      [-v|--verbose] [-V|--version] [-l|--list-profiles]
                      [-u|--user ARG] [-p|--aws-profile ARG] [-r|--region ARG]
                      [-c|--config-file ARG] [-k|--keep-reloading] ([--no-ecr] |
                      [--ecr]))
  Login to AWS via Okta/SAML (source:
  https://github.com/saksdirect/okta-aws-login) Default config file:
  "/Users/yourhome/.okta-aws-login.json"

Available options:
  -h,--help                Show this help text
  --configure              Configure AWS profile given an Okta 'embed' link
  -p,--aws-profile ARG     Name of the associated AWS profile.
  -l,--okta-embed-link ARG Okta AWS app 'embed' link, ask your Okta
                           administrator.
  -d,--default             User this profile by default.
  -r,--ecr                 Enable Docker login to ECR registry by default.
  -c,--config-file ARG     Use alternative config
                           file. (default: "/Users/yourhome/.okta-aws-login.json")
  -s,--session-duration ARG
                           STS session duration, seconds. You can provide a
                           value from 900 seconds (15 minutes) up to the maximum
                           session duration setting for the role. Please
                           coordinate with your AWS administrator.
  -v,--verbose             Be verbose.
  -V,--version             Print version and exit.
  -l,--list-profiles       List available AWS profiles and exit.
  -u,--user ARG            User name.
  -p,--aws-profile ARG     AWS profile. Defaults to value of AWS_PROFILE env
                           var, then to default config entry.
  -r,--region ARG          AWS region. (default: us-east-1)
  -c,--config-file ARG     Use alternative config
                           file. (default: "/Users/yourhome/.okta-aws-login.json")
  -k,--keep-reloading      Keep reloading session token hourly (that's the max
                           TTL at the moment). This only works well on a trusted
                           network where you don't need MFA.
  --no-ecr                 Skip Docker login to ECR registry, default is in the
                           config.
  --ecr                    Attempt Docker login to ECR registry, default is in
                           the config.

Log in using default AWS profile, you'll be prompted for user name / password: 

  $ okta-aws-login 

Specify user name and keep reloading session: 

  $ okta-aws-login --user my-okta-user-name --keep-reloading 

Log in with more than one AWS profile: 

  $ okta-aws-login --user my-okta-user-name --aws-profile my-aws-profile1 --aws-profile my-aws-profile2 

Skip ECR login (note that you can set default behavior in the config file) 

  $ okta-aws-login --no-ecr --user my-okta-user-name --aws-profile my-aws-profile1

```


# Development

To build it from source you need [haskell stack](https://docs.haskellstack.org/en/stable/README/).
While not strictly required, `Makefile` tries to compress generated binaries with [upx](https://upx.github.io/) (`brew install upx`).
Optionally you also need [hlint](https://github.com/ndmitchell/hlint) (`stack install hlint`).


Okta [API](http://developer.okta.com/docs/api/resources/authn.html).

Okta official CLI [inspiration](https://github.com/oktadeveloper/okta-aws-cli-assume-role)
