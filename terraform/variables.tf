# Cuisine Code Terraform variables
# Copyright (c) 2025 Aidan Pace

variable "do_token" {
  description = "DigitalOcean API token"
  type        = string
}

variable "ssh_key_id" {
  description = "SSH key ID for DigitalOcean"
  type        = string
}

variable "ssh_private_key_path" {
  description = "Path to SSH private key"
  type        = string
  default     = "~/.ssh/id_rsa"
}

variable "region" {
  description = "DigitalOcean region"
  type        = string
  default     = "nyc3"
}

variable "app_domain" {
  description = "Domain for the application"
  type        = string
  default     = "cc.fx.net"
}
