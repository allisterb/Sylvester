#!/bin/bash
curl -SL --output /opt/microsoft/ropen/3.5.3/lib64/R/lib/libTensorFlow.so https://allisterb-sylvester.s3.us-east-2.amazonaws.com/libtensorflow.so
curl -SL --output /opt/microsoft/ropen/3.5.3/lib64/R/lib/libtensorflow_framework.so.2 https://allisterb-sylvester.s3.us-east-2.amazonaws.com/libtensorflow_framework.so.2.0.0 
export LIBRARY_PATH=$LIBRARY_PATH:/home/nbuser/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/nbuser/