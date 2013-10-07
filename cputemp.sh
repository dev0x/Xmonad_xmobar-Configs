#!/bin/bash
sensors | grep "CPU Temperature" | awk '{print $3}'
