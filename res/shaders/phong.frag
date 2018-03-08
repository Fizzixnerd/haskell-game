#version 450 core

#define MAX_POINT_LIGHTS 4

layout (row_major) uniform;

in VS_OUT {
  vec3 pos;
  vec3 norm;
  vec3 view;
  vec2 uv;
  vec3[MAX_POINT_LIGHTS] light;
  float[MAX_POINT_LIGHTS] intensity;
} fs_in;

layout (std140, binding = 0) uniform Camera {
  layout (offset = 0)   mat4 mvp;
  layout (offset = 64)  mat4 mv;
  layout (offset = 128) mat4 p;
} camera;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  layout (offset = 0) PointLight[MAX_POINT_LIGHTS] lights;
  layout (offset = 32 * MAX_POINT_LIGHTS) int num;
} point_lights;

layout (std140, binding = 2) uniform Material {
  layout (offset = 0) vec4 diffuse_color;
  layout (offset = 16) vec4 ambient_color;
  layout (offset = 32) vec4 specular_color;
  layout (offset = 48) float specular_strength;
  layout (offset = 52) float specular_exponent;
} material;

layout (binding = 0) uniform sampler2D diffuse_sampler;

out vec3 color;

void main() {
  vec3 N = normalize(fs_in.norm);
  vec3 V = normalize(fs_in.view);

  vec3 diffuse = vec3(0);
  vec3 specular = vec3(0);
  int i;
  for (i = 0; i < min(MAX_POINT_LIGHTS, point_lights.num); i++) {
    vec3 L = normalize(fs_in.light[i]);
    vec3 R = reflect(-L, N);

    diffuse += max(dot(N, L), 0.0) * material.diffuse_color.rgb * fs_in.intensity[i];
    specular += pow(max(dot(R, V), 0.0), material.specular_exponent) *
      material.specular_strength * material.specular_color.rgb * pow(fs_in.intensity[i], 1);
  }
  vec3 ambient = material.ambient_color.rgb;

  color = texture(diffuse_sampler, fs_in.uv).rgb
    + 0.2 * diffuse + pow(0.2, 2) * specular
    + ambient * 0.05;
}
