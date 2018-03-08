#version 450 core

#define MAX_POINT_LIGHTS 4
layout (row_major) uniform;

layout (std140, binding = 0) uniform Camera {
  mat4 mvp;
  mat4 mv;
  mat4 p;
} camera;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (std140, binding = 2) uniform Material {
  vec4 diffuse_color;
  vec4 ambient_color;
  vec4 specular_color;
  float specular_strength;
  float specular_exponent;
} material;

in VS_OUT {
  vec3 pos;
  vec3 norm;
  vec3 view;
  vec2 uv;
  vec3 light;
  float intensity;
} fs_in;

layout (binding = 0) uniform sampler2D diffuse_sampler;

out vec3 color;

void main() {
  vec3 N = normalize(fs_in.norm);
  vec3 V = normalize(fs_in.view);

  vec3 diffuse = vec3(0);
  vec3 specular = vec3(0);
  int i;
  for (i = 0; i < min(MAX_POINT_LIGHTS, point_lights.num); i++) {
    vec3 L = normalize(fs_in.light);
    vec3 R = reflect(-L, N);

    diffuse += max(dot(N, L), 0.0) * material.diffuse_color.rgb;
    specular += pow(max(dot(R, V), 0.0), material.specular_exponent) *
      material.specular_strength * material.specular_color.rgb;
  }
  vec3 ambient = material.ambient_color.rgb;

  color = texture(diffuse_sampler, fs_in.uv).rgb  + diffuse + specular + ambient;
}
