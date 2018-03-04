#version 450 core

layout (location = 0) in vec3 vertexPos;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexUV;

layout (location = 0) uniform mat4 MVP;

out VS_OUT {
  vec2 UV;
} vs_out;

void main() {
  gl_Position = MVP * vec4(vertexPos, 1);
  vs_out.UV = vertexUV;
}
