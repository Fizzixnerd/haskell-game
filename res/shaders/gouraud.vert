#version 450 core

#define MAX_POINT_LIGHTS 128

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

layout (std140, binding = 0) uniform Camera {
  mat4 mvp;
  mat4 mv;
  mat4 p;
} camera;

struct PointLight {
  vec3 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  // xyz is position; w is intensity.
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (std140, binding = 2) uniform Material {
  vec3 diffuse_color;
  float specular_strength;
  vec3 ambient_color;
  float specular_exponent;
  vec3 specular_color;
} material;

out VS_OUT {
  vec3 lighting;
  vec2 uv;
} vs_out;

void main() {
  vec4 pos = camera.mv * vec4(position, 1);

  vec3 view_vector = normalize(- pos.xyz);

  // Calculate the normal in view space.
  vec3 norm = normalize(mat3(camera.mv) * normal);

  vec3 diffuse = {0, 0, 0};
  vec3 specular = {0, 0, 0};
  int i;
  for (i = 0; i < point_lights.num; i++) {
    vec3 light_vector_normed = normalize(point_lights.lights[i].position - pos.xyz);
    vec3 light_vector = point_lights.lights[i].intensity * light_vector_normed;
    vec3 reflect_vector = reflect(-light_vector_normed, norm);

    diffuse += max(dot(norm, light_vector), 0.0) * material.diffuse_color;
    specular += pow(max(dot(reflect_vector, view_vector), 0.0),
                    material.specular_exponent) *
      material.specular_strength * material.specular_color;
  }

  vs_out.lighting = material.ambient_color + diffuse + specular;
  vs_out.uv = uv;
  gl_Position = camera.p * pos;
}
