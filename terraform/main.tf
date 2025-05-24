# Cuisine Code Terraform configuration
# Copyright (c) 2025 Aidan Pace

terraform {
  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.0"
    }
  }
}

provider "digitalocean" {
  token = var.do_token
}

# Variables
variable "do_token" {
  description = "DigitalOcean API token"
  type        = string
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

# Create VPC
resource "digitalocean_vpc" "cuisine_vpc" {
  name     = "cuisine-code-vpc"
  region   = var.region
  ip_range = "10.10.0.0/16"
}

# Create Droplets
resource "digitalocean_droplet" "cuisine_app" {
  count    = 2
  name     = "cuisine-app-${count.index + 1}"
  region   = var.region
  vpc_uuid = digitalocean_vpc.cuisine_vpc.id
  size     = "s-2vcpu-4gb"
  image    = "freebsd-14-x64"
  ssh_keys = [var.ssh_key_id]
  
  tags = ["app", "cuisine-code"]
  
  connection {
    host        = self.ipv4_address
    user        = "freebsd"
    type        = "ssh"
    private_key = file(var.ssh_private_key_path)
  }
  
  provisioner "remote-exec" {
    inline = [
      "pkg update",
      "pkg install -y docker docker-compose git",
      "sysrc docker_enable=YES",
      "service docker start",
      "mkdir -p /opt/cuisine-code",
    ]
  }
}

# Create Database Cluster
resource "digitalocean_database_cluster" "cuisine_db" {
  name       = "cuisine-db-cluster"
  engine     = "pg"
  version    = "14"
  size       = "db-s-1vcpu-2gb"
  region     = var.region
  node_count = 1
  private_network_uuid = digitalocean_vpc.cuisine_vpc.id
}

resource "digitalocean_database_db" "cuisine_database" {
  cluster_id = digitalocean_database_cluster.cuisine_db.id
  name       = "cuisine_code"
}

resource "digitalocean_database_user" "cuisine_user" {
  cluster_id = digitalocean_database_cluster.cuisine_db.id
  name       = "cuisine"
}

# Create Load Balancer
resource "digitalocean_loadbalancer" "cuisine_lb" {
  name   = "cuisine-lb"
  region = var.region
  vpc_uuid = digitalocean_vpc.cuisine_vpc.id
  
  forwarding_rule {
    entry_port     = 80
    entry_protocol = "http"
    target_port     = 80
    target_protocol = "http"
  }
  
  forwarding_rule {
    entry_port     = 443
    entry_protocol = "https"
    target_port     = 80
    target_protocol = "http"
    certificate_id = digitalocean_certificate.cuisine_cert.id
  }
  
  healthcheck {
    port     = 80
    protocol = "http"
    path     = "/"
  }
  
  droplet_ids = digitalocean_droplet.cuisine_app[*].id
}

# Create Domain and DNS Records
resource "digitalocean_domain" "cuisine_domain" {
  name = var.app_domain
}

resource "digitalocean_record" "cuisine_a" {
  domain = digitalocean_domain.cuisine_domain.name
  type   = "A"
  name   = "@"
  value  = digitalocean_loadbalancer.cuisine_lb.ip
  ttl    = 300
}

resource "digitalocean_record" "cuisine_www" {
  domain = digitalocean_domain.cuisine_domain.name
  type   = "CNAME"
  name   = "www"
  value  = "@"
  ttl    = 300
}

# Create SSL Certificate
resource "digitalocean_certificate" "cuisine_cert" {
  name    = "cuisine-cert"
  type    = "lets_encrypt"
  domains = [var.app_domain, "www.${var.app_domain}"]
}

# Project outputs
output "app_url" {
  value = "https://${var.app_domain}"
}

output "database_uri" {
  value     = digitalocean_database_cluster.cuisine_db.uri
  sensitive = true
}

output "lb_ip" {
  value = digitalocean_loadbalancer.cuisine_lb.ip
}
