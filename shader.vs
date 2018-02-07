#version 450 core

layout (location = 0) in vec4 pos;
layout (location = 1) in vec2 vertexUV;

uniform mat4 MVP;

out VS_OUT {
  vec2 UV;
} vs_out;

void main() {
  gl_Position = MVP * pos;
  vs_out.UV = vertexUV;
}
