#version 450 core

in VS_OUT {
  vec2 UV;
} vs_out;

layout (binding = 0) uniform sampler2D texSampler;

out vec3 color;

void main() {
  color = texture(texSampler, vs_out.UV).rgb;
}
