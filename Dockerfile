FROM fsprojects/ifsharp:latest

	
# Install TensorFlow

RUN curl -SL --output libtensorflow.so https://allisterb-sylvester.s3.us-east-2.amazonaws.com/libtensorflow.so \
&& curl -SL --output libtensorflow_framework.so.2.0.0 https://allisterb-sylvester.s3.us-east-2.amazonaws.com/libtensorflow_framework.so.2.0.0 \
&& cp libtensorflow.so /usr/local/lib/libTensorFlow && cp libtensorflow.so /usr/local/lib/TensorFlow && cp libtensorflow.so /usr/local/lib/TensorFlow.so \
&& cp libtensorflow_framework.so.2.0.0 /usr/local/lib/ && cp libtensorflow_framework.so.2.0.0 /usr/local/lib/libtensorflow_framework.so.2

ENV LIBRARY_PATH="${LIBRARY_PATH}:/usr/local/lib"

ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"

RUN echo "LIBRARY_PATH=$LIBRARY_PATH"

RUN echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"