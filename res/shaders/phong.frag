#version 450 core

#define MAX_POINT_LIGHTS 128

in VS_OUT {
  vec3 pos;
  vec3 norm;
  vec3 view;
  vec2 uv;
  vec3[MAX_POINT_LIGHTS] light;
  float[MAX_POINT_LIGHTS] intensity;
} fs_in;

layout (std140, binding = 0) uniform Camera {
  layout (offset = 0) mat4 mvp;
  layout (offset = 64) mat4 mv;
  layout (offset = 128) mat4 p;
} camera;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (std140, binding = 2, align = 16) uniform Material {
  layout (offset = 192) vec4 diffuse_color;
  layout (offset = 208) vec4 ambient_color;
  layout (offset = 224) vec4 specular_color;
  layout (offset = 240) float specular_strength;
  layout (offset = 244) float specular_exponent;
} material;

out vec3 color;

layout (binding = 0) uniform sampler2D diffuse_sampler;

void main() {
  vec3 N = normalize(fs_in.norm);
  vec3 V = normalize(fs_in.view);

  vec3 diffuse = vec3(0);
  vec3 specular = vec3(0);
  int i;
  for (i = 0; i < min(MAX_POINT_LIGHTS, point_lights.num); i++) {
    vec3 L = normalize(fs_in.light[i]);
    vec3 R = reflect(-L, N);

    diffuse += max(dot(N,L), 0) * material.diffuse_color.rgb;
    specular += pow(max(dot(R, V), 0), material.specular_exponent) *
      material.specular_strength * material.specular_color.rgb;
  }
  vec3 ambient = material.ambient_color.rgb;

  color = texture(diffuse_sampler, fs_in.uv).rgb + diffuse + specular + ambient;
}
