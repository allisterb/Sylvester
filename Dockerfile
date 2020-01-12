FROM fsprojects/ifsharp:latest

	
# Install TensorFlow

RUN curl -SL --output libtensorflow.so https://allisterb-sylvester.s3.us-east-2.amazonaws.com/libtensorflow.so \
&& curl -SL --output libtensorflow_framework.so.2.0.0 https://allisterb-sylvester.s3.us-east-2.amazonaws.com/libtensorflow_framework.so.2.0.0
RUN cp libtensorflow.so libTensorFlow.so
RUN cp libtensorflow_framework.so.2.0.0 libtensorflow_framework.so.2
ENV LIBRARY_PATH="/IfSharp"

ENV LD_LIBRARY_PATH="/IfSharp"

RUN echo "LIBRARY_PATH=$LIBRARY_PATH"

RUN echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
