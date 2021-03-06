# Login to AWS account via Okta (SAML) from the command line

Logs in to ECS at the same time. Populates `$HOME/.aws/credentials` and `$HOME/.docker/config.json`.

[Releases](https://github.com/andreyk0/okta-aws-login/releases)


```bash
Login to AWS via Okta/SAML.

Usage: okta-aws-login [-v|--verbose] [-u|--user ARG] [-p|--aws-profile ARG]
                      [-r|--region ARG] [-c|--config-file ARG]
                      [-k|--keep-reloading]

Available options:
  -h,--help                Show this help text
  -v,--verbose             Be verbose.
  -u,--user ARG            User name.
  -p,--aws-profile ARG     AWS profile. Default config entry will be used if not
                           given.
  -r,--region ARG          AWS region. (default: us-east-1)
  -c,--config-file ARG     Use alternative config
                           file. (default: "$HOME/.okta-aws-login.json")
  -k,--keep-reloading      Keep reloading session token hourly (that's the max
                           TTL at the moment). This only works well on a trusted
                           network where you don't need MFA.
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

To build it from source you need [haskell stack](https://docs.haskellstack.org/en/stable/README/)

Okta [API](http://developer.okta.com/docs/api/resources/authn.html).

Okta official CLI [inspiration](https://github.com/oktadeveloper/okta-aws-cli-assume-role)
