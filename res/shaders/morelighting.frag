#version 450 core

#define MAX_POINT_LIGHTS 4

layout (row_major, std140) uniform;

in VS_OUT {
  vec2 uv;
  vec3 view;
  vec3[MAX_POINT_LIGHTS] light;
} fs_in;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (binding = 0) uniform Camera {
  mat4 mvp;
  mat4 mv;
  mat4 p;
} camera;

layout (binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (binding = 2) uniform Material {
  vec4 diffuse_color;
  vec4 ambient_color;
  vec4 specular_color;
  float specular_strength;
  float specular_exponent;
} material;

layout (binding = 0) uniform sampler2D tex_color;
layout (binding = 5) uniform sampler2D tex_normal;

out vec4 color;

void main() {
  vec3 V = normalize(fs_in.view);

  // TODO: Why is this x2 - 1?
  vec3 N = normalize(texture(tex_normal, fs_in.uv)).rgb * 2 - vec3(1);
  int i;
  vec3 diffuse = vec3(0);
  vec3 specular = vec3(0);
  for (i = 0; i < min(point_lights.num, MAX_POINT_LIGHTS); i++) {
    vec3 L = normalize(fs_in.light[i].xyz);
    vec3 R = reflect(-L, N);

    diffuse += material.diffuse_color.rgb * max(dot(N, L), 0.0) * 0.2;
    specular += material.specular_color.rgb * max(pow(dot(R, V), material.specular_exponent), 0.0) * material.specular_strength;
  }
  vec3 ambient = material.ambient_color.rgb;
  color = vec4(texture(tex_color, fs_in.uv).rgb +
               diffuse * 0.2 +
               ambient * 0.05 +
               specular * pow(0.2, 2), 1.0);
}