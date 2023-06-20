locals {
  domain      = "blog.sulami.xyz"
  site_bucket = "blog.sulami.xyz"
}

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.1"
    }
  }

  required_version = ">= 1.4"
}

resource "aws_cloudfront_distribution" "cdn" {
  aliases = [local.domain]

  default_cache_behavior {
    allowed_methods = ["GET", "HEAD"]
    cache_policy_id = "658327ea-f89d-4fab-a63d-7e88639e58f6"
    cached_methods  = ["GET", "HEAD"]
    compress        = "true"
    default_ttl     = "0"

    function_association {
      event_type   = "viewer-request"
      function_arn = resource.aws_cloudfront_function.append-index-html.arn
    }

    target_origin_id       = resource.aws_s3_bucket.site.bucket_domain_name
    viewer_protocol_policy = "redirect-to-https"
  }

  enabled         = "true"
  http_version    = "http2and3"
  is_ipv6_enabled = "true"

  origin {
    connection_attempts      = "3"
    connection_timeout       = "10"
    domain_name              = resource.aws_s3_bucket.site.bucket_domain_name
    origin_access_control_id = "E11KDC2SQFNCI1"
    origin_id                = resource.aws_s3_bucket.site.bucket_domain_name
  }

  price_class = "PriceClass_All"

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  retain_on_delete = "false"

  viewer_certificate {
    acm_certificate_arn = resource.aws_acm_certificate.blog.arn
    ssl_support_method  = "sni-only"
  }
}

resource "aws_acm_certificate" "blog" {
  domain_name       = "blog.sulami.xyz"
  validation_method = "DNS"
}

resource "aws_cloudfront_function" "append-index-html" {
  name    = "Append-indexhtml"
  runtime = "cloudfront-js-1.0"

  code = <<CODE
function handler(event) {
    var request = event.request;
    var uri = request.uri;

    // Check whether the URI is missing a file name.
    if (uri.endsWith('/')) {
        request.uri += 'index.html';
    }
    // Check whether the URI is missing a file extension.
    else if (!uri.includes('.')) {
        request.uri += '/index.html';
    }

    return request;
}
CODE
}

resource "aws_s3_bucket" "site" {
  bucket              = local.site_bucket
  force_destroy       = "false"
  object_lock_enabled = "false"
}

resource "aws_s3_bucket_ownership_controls" "site" {
  bucket = aws_s3_bucket.site.id

  rule {
    object_ownership = "BucketOwnerPreferred"
  }
}

resource "aws_s3_bucket_policy" "site" {
  bucket = resource.aws_s3_bucket.site.bucket
  policy = <<POLICY
{
  "Id": "PolicyForCloudFrontPrivateContent",
  "Statement": [
    {
      "Action": "s3:GetObject",
      "Condition": {
        "StringEquals": {
          "AWS:SourceArn": "${resource.aws_cloudfront_distribution.cdn.arn}"
        }
      },
      "Effect": "Allow",
      "Principal": {
        "Service": "cloudfront.amazonaws.com"
      },
      "Resource": "${resource.aws_s3_bucket.site.arn}/*",
      "Sid": "AllowCloudFrontServicePrincipal"
    }
  ],
  "Version": "2008-10-17"
}
POLICY
}
