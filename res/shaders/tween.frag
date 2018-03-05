#version 450 core

in VS_OUT {
  vec4 lighting;
  vec2 uv;
} fs_in;

layout (binding = 0) uniform sampler2D diffuse_sampler;

out vec3 color;

void main() {
  color = texture(diffuse_sampler, fs_in.uv).rgb;
}
